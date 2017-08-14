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
    let eventCode' = FFI.intToProcMatchEventType (fromInteger eventCode)
                                        -- unknown event type, skip the
                                        -- cache
        f' = f pid eventCode progName
        pid' = fromInteger pid
        in case eventCode' of Nothing -> f'
                              -- only write to the cache on ProcStart
                              Just (FFI.ProcStart) -> do
                                  modifyIORef' cache (insert pid' progName)
                                  f'
                              Just (FFI.ProcEnd) -> do
                                  hasPid <- Map.lookup pid' <$> readIORef cache
                                  -- if we don't have the PID in the cache,
                                  -- filter out the PROC_END event
                                  case hasPid of Nothing -> return ()
                                                 Just _  -> f'
                              _ -> f'

