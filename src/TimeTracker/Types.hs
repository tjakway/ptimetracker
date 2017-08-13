module TimeTracker.Types where

import TimeTracker.FFI
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Control.Monad.State.Strict

data ProgramLoggerS = ProgramLoggerS {
                    eventCallbacks :: [CEventCallbackFunPtr],
                    errorCallbacks :: [ErrorCallbackFunPtr],
                    stopListeningCallbacks :: [StopListeningCallbackFunPtr],
                    apiState :: APIStatePtr
                    }

emptyProgramLoggerS :: ProgramLoggerS
emptyProgramLoggerS = ProgramLoggerS {
    eventCallbacks = [],
    errorCallbacks = [],
    stopListeningCallbacks = [],
    apiState = nullPtr
}

type ProgramLoggerM a = StateT ProgramLoggerS IO a

getAPIState :: ProgramLoggerM APIStatePtr
getAPIState = apiState <$> get


cleanupProgramLogger :: ProgramLoggerM ()
cleanupProgramLogger = get >>= \s -> liftIO $ do
        freeAPIState (apiState s)
        mapM_ freeHaskellFunPtr (eventCallbacks s)
        mapM_ freeHaskellFunPtr (errorCallbacks s)
        mapM_ freeHaskellFunPtr (stopListeningCallbacks s)



-- | probably only used for debugging since the final logger state isn't
-- useful
execProgramLogger :: ProgramLoggerM a -> IO (a, ProgramLoggerS)
execProgramLogger stateAction = do
        apiStatePtr <- initializeAPIState

        let start = emptyProgramLoggerS { apiState = apiStatePtr }
            stateAction' = stateAction >>= (\x -> cleanupProgramLogger >> return x)
        (res, finalState) <- runStateT stateAction' start

        return (res, finalState)

runProgramLogger :: ProgramLoggerM a -> IO a
runProgramLogger x = (execProgramLogger x) >>= (\(a, _) -> return a)


