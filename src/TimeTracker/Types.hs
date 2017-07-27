module TimeTracker.Types where

import TimeTracker.FFI
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Control.Monad.State

data ProgramLoggerS = ProgramLoggerS {
                    eventCallbacks :: [EventCallbackFunPtr],
                    errorCallbacks :: [ErrorCallbackFunPtr],
                    stopListeningCallbacks :: [StopListeningCallback],
                    ioF :: IO (),
                    apiState :: APIStatePtr
                    }

emptyProgramLoggerS :: ProgramLoggerS
emptyProgramLoggerS = ProgramLoggerS {
    eventCallbacks = [],
    errorCallbacks = [],
    stopListeningCallbacks = [],
    ioF = return (),
    apiState = nullPtr
}

type ProgramLoggerM a = State ProgramLoggerS a

cleanupProgramLogger :: ProgramLoggerS -> IO ()
cleanupProgramLogger s = do
        freeAPIState (apiState s)
        mapM_ freeHaskellFunPtr (eventCallbacks s)
        mapM_ freeHaskellFunPtr (errorCallbacks s)
        mapM_ freeHaskellFunPtr (stopListeningCallbacks s)

runProgramLogger :: ProgramLoggerM a -> IO a
runProgramLogger stateAction = do
        apiStatePtr <- initializeAPIState

        let start = emptyProgramLoggerS { apiState = apiStatePtr }
            (res, finalState) = runState stateAction start

        cleanupProgramLogger finalState
        return res
