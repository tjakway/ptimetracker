module TimeTracker.Types where

import TimeTracker.FFI
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Control.Monad

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

-- Adapted from the LlvmM monad in GHC
-- see compiler/llvmGen/LlvmCodeGen/Base.hs
newtype ProgramLoggerM a = ProgramLoggerM { runProgramLoggerM' :: ProgramLoggerS -> IO (a, ProgramLoggerS) }

instance Functor ProgramLoggerM where
    fmap f m = ProgramLoggerM $ \s -> do 
            (x, s') <- runProgramLoggerM' m s
            return (f x, s')

instance Applicative ProgramLoggerM where
    pure x = ProgramLoggerM $ \s -> return (x, s)
    (<*>) = ap

instance Monad ProgramLoggerM where
    return = pure
    m >>= f  = ProgramLoggerM $ \s -> do 
                (x, s') <- runProgramLoggerM' m s 
                runProgramLoggerM' (f x) s'

getState :: ProgramLoggerM ProgramLoggerS
getState = ProgramLoggerM $ \s -> return (s, s)

getAPIState :: ProgramLoggerM APIStatePtr
getAPIState = apiState <$> getState


liftS :: IO a -> ProgramLoggerM a
liftS f = ProgramLoggerM $ \s -> f >>= (\a -> return (a, s))

cleanupProgramLogger :: ProgramLoggerM ()
cleanupProgramLogger = ProgramLoggerM $ \s -> do
        freeAPIState (apiState s)
        mapM_ freeHaskellFunPtr (eventCallbacks s)
        mapM_ freeHaskellFunPtr (errorCallbacks s)
        mapM_ freeHaskellFunPtr (stopListeningCallbacks s)
        return ((), s)



-- | probably only used for debugging since the final logger state isn't
-- useful
execProgramLogger :: ProgramLoggerM a -> IO (a, ProgramLoggerS)
execProgramLogger stateAction = do
        apiStatePtr <- initializeAPIState

        let start = emptyProgramLoggerS { apiState = apiStatePtr }
            stateAction' = stateAction >>= (\x -> cleanupProgramLogger >> return x)
        (res, finalState) <- runProgramLoggerM' stateAction' start

        return (res, finalState)

runProgramLogger :: ProgramLoggerM a -> IO a
runProgramLogger x = (execProgramLogger x) >>= (\(a, _) -> return a)


