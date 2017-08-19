{-# LANGUAGE ScopedTypeVariables #-}
module TimeTracker.PidCache 
(
PidCache,
initPidCache,
withPidCache
)
where

import Data.Map.Strict as Map
import Data.IORef
import TimeTracker.Interface
import qualified TimeTracker.FFI as FFI

type PidCache = Map.Map FFI.PidT String

initPidCache :: IO (IORef PidCache)
initPidCache = newIORef Map.empty

-- | wraps the EventCallback and filters PROC_END events for which we don't
-- know the program name
withPidCache :: IORef PidCache -> EventCallback -> EventCallback
withPidCache cache f = \pid eventCode progName ->
    let eventCode' = FFI.intToProcMatchEventType (fromIntegral eventCode)
                                        -- unknown event type, skip the
                                        -- cache
        f' = f pid eventCode
        pid' = fromIntegral pid
        isProcStartOrTick e = e == FFI.ProcStart || 
                                (e /= FFI.ProcEnd &&
                                 e /= FFI.Other   &&
                                 e /= FFI.NoEvent)

        -- atomic version that matches the signature of modifyIORef'
        atomicModifyIORefFst' :: IORef a -> (a -> a) -> IO ()
        atomicModifyIORefFst' ref g = atomicModifyIORef' ref (\a -> (g a, ()))

        in case eventCode' of Nothing -> f' progName
                              -- write to the cache on proc start or tick
                              -- events
                              Just event
                                | isProcStartOrTick event -> do
                                    atomicModifyIORefFst' cache (insert pid' progName)
                                    f' progName
                                -- if the event is PROC_END the passed
                                -- progName will be the empty string
                                -- in that case we have to look it up
                                -- if we can't find it, don't invoke the
                                -- EventCallback
                                | event == FFI.ProcEnd -> do
                                    hasPid <- Map.lookup pid' <$> readIORef cache
                                    -- if we don't have the PID in the cache,
                                    -- filter out the PROC_END event
                                    case hasPid of Nothing -> return ()
                                                   Just foundProgName  -> do
                                                       atomicModifyIORefFst' cache (Map.delete pid')
                                                       f' foundProgName

