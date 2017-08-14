module TimeTracker.PidCache where

import Data.Map.Strict as Map
import Data.IORef
import TimeTracker.Interface
import qualified TimeTracker.FFI as FFI

type PidCache = Map.Map FFI.PidT String

initPidCache :: IO (IORef PidCache)
initPidCache = newIORef Map.empty

withPidCache :: IORef PidCache -> EventCallback -> EventCallback
withPidCache cache f = \pid eventCode progName ->
    let eventCode' = FFI.intToProcMatchEventType (fromInteger eventCode)
                                        -- unknown event type, skip the
                                        -- cache
        f' = f pid eventCode progName
        in case eventCode' of Nothing -> f'
                              -- only write to the cache on ProcStart
                              Just (FFI.ProcStart) -> do
                                  let pid' = fromInteger pid
                                  modifyIORef' cache (insert pid' progName)
                                  f'
                              _ -> f'

