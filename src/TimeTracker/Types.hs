module TimeTracker.Types where

import TimeTracker.FFI
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Control.Monad

data ProgramLoggerS = ProgramLoggerS {
                    eventCallbacks :: [EventCallbackFunPtr],
                    errorCallbacks :: [ErrorCallbackFunPtr],
                    stopListeningCallbacks :: [StopListeningCallback],
                    apiState :: APIStatePtr
                    }

emptyProgramLoggerS :: ProgramLoggerS
emptyProgramLoggerS = ProgramLoggerS {
    eventCallbacks = [],
    errorCallbacks = [],
    stopListeningCallbacks = [],
    apiState = nullPtr
}

-- Adapted from the LlvmM monad in GHC
-- see compiler/llvmGen/LlvmCodeGen/Base.hs
newtype ProgramLoggerM a = ProgramLoggerM { runProgramLoggerM :: ProgramLoggerS -> IO (a, ProgramLoggerS) }

instance Functor ProgramLoggerM where
    fmap f m = ProgramLoggerM $ \s -> do 
            (x, s') <- runProgramLoggerM m s
            return (f x, s')

instance Applicative ProgramLoggerM where
    pure x = ProgramLoggerM $ \s -> return (x, s)
    (<*>) = ap

instance Monad ProgramLoggerM where
    return = pure
    m >>= f  = ProgramLoggerM $ \s -> do 
                (x, s') <- runProgramLoggerM m s 
                runProgramLoggerM (f x) s'

getState :: ProgramLoggerM ProgramLoggerS
getState = ProgramLoggerM $ \s -> return (s, s)


cleanupProgramLogger :: ProgramLoggerM ()
cleanupProgramLogger = ProgramLoggerM $ \s -> do
        freeAPIState (apiState s)
        mapM_ freeHaskellFunPtr (eventCallbacks s)
        mapM_ freeHaskellFunPtr (errorCallbacks s)
        mapM_ freeHaskellFunPtr (stopListeningCallbacks s)
        return ((), s)

{-runProgramLogger :: ProgramLoggerM a -> IO a
runProgramLogger stateAction = do
        apiStatePtr <- initializeAPIState

        let start = emptyProgramLoggerS { apiState = apiStatePtr }
            (res, finalState) = runState stateAction start

        cleanupProgramLogger finalState
        return res
        -}
