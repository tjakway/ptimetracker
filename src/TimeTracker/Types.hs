module TimeTracker.Types where

import TimeTracker.FFI
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)

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

newtype ProgramLoggerM a = ProgramLoggerM { runProgramLogger :: ProgramLoggerS -> IO (a, ProgramLoggerS) }

instance Monad ProgramLoggerM where
        --return x = return $ \s -> (x, s)
        (>>=) m f = \s -> do
            (r, s') <- m s
            f s'
        {-(>>=) a f = \s -> do
            (a', s') <- a s
            (x, y)   <- f a'
            y s'-}

-- Adapted from the LlvmM monad in GHC
-- see compiler/llvmGen/LlvmCodeGen/Base.hs
instance Functor ProgramLoggerM where
    fmap f m = ProgramLoggerM $ \s -> do 
            (x, s') <- runProgramLogger m s
            return (f x, s')

instance Applicative ProgramLoggerM where
    pure x = ProgramLoggerM $ \s -> return (x, s)
    (<*>) = ap

instance Monad ProgramLoggerM where
    m >>= f  = LlvmM $ \s -> do (x, env') <- runProgramLoggerM m s 
                                  runLlvmM (f x) env'



getState :: ProgramLoggerM ProgramLoggerS
getState s = return (s, s) 

cleanupProgramLogger :: ProgramLoggerM ()
cleanupProgramLogger s = do
        freeAPIState (apiState s)
        mapM_ freeHaskellFunPtr (eventCallbacks s)
        mapM_ freeHaskellFunPtr (errorCallbacks s)
        mapM_ freeHaskellFunPtr (stopListeningCallbacks s)

{-runProgramLogger :: ProgramLoggerM a -> IO a
runProgramLogger stateAction = do
        apiStatePtr <- initializeAPIState

        let start = emptyProgramLoggerS { apiState = apiStatePtr }
            (res, finalState) = runState stateAction start

        cleanupProgramLogger finalState
        return res
        -}
