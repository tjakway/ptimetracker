module TimeTracker.Types where

import TimeTracker.FFI
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Control.Monad.State.Strict
import Foreign.C.Types (CInt)
import System.Posix.Types
import System.Posix.IO

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

redirectNativeLog :: FilePath -> (APIStatePtr -> CInt -> IO ()) -> ProgramLoggerM ()
redirectNativeLog dest logFunction = do
        apiStatePtr <- getAPIState
        fd <- liftIO $ getFdCInt <$> openFd dest WriteOnly Nothing defaultFileFlags
        liftIO . logFunction apiStatePtr $ fd
    where getFdCInt (Fd i) = i

-- | redirect native logging -> /dev/null
setupNativeLogging :: ProgramLoggerM ()
setupNativeLogging = redirectNativeLog "/dev/null" apiSetOutFd

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
            stateAction' = setupNativeLogging >> stateAction >>= (\x -> cleanupProgramLogger >> return x)
        (res, finalState) <- runStateT stateAction' start

        return (res, finalState)

runProgramLogger :: ProgramLoggerM a -> IO a
runProgramLogger x = (execProgramLogger x) >>= (\(a, _) -> return a)


