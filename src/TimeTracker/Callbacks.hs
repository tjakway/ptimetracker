module TimeTracker.Callbacks where

-- TODO: remove unused imports
import System.IO
import System.Exit
import qualified System.Log.Logger as Logger
import Control.Monad (when)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import System.Posix.User
import TimeTracker.Interface
import TimeTracker.Types
import TimeTracker.IO.Database
import TimeTracker.Config.ConfigTypes
import TimeTracker.PidCache
import qualified TimeTracker.FFI as FFI


-- | run a certain number of times
countCallback :: Int -> IO FFI.StopListeningCallback
countCallback limit = do
            counter <- newIORef 0
            let continueCallback' = \_ -> do
                    modifyIORef' counter (+1)
                    count <- readIORef counter
                    putStrLn ("listenUntilCallback' called, count is " ++ (show count))
                    if count > limit then return (boolToCInt False)
                                 else return (boolToCInt True)
            return continueCallback'


logCallback :: (String -> IO ()) -> Integer -> Integer -> String -> DbMonad ()
logCallback logError pid procEventTypeInt progName = 
        let procEventData = FFI.intToProcMatchEventType (fromInteger procEventTypeInt) >>= 
                   \x -> case x of FFI.Other     -> Just Other
                                   FFI.NoEvent   -> Just NoEvent
                                   FFI.ProcStart -> Just . ProcStart . fromInteger $ pid
                                   FFI.ProcEnd   -> Just . ProcEnd . fromInteger $ -1 -- TODO: need to get the actual exit code
        in do
            -- TODO: fix line length
            case procEventData of Nothing -> liftIO $ logError ("Received unknown event type from progName" ++ progName)
                                  Just ev -> insertProcEvents [(ev, progName)] >> commitDb

dbMonadAction :: (String -> IO ()) -> DbMonad ()
dbMonadAction logError = 
        let procRegex = ".*"
            cwdRegex = ".*"
            procM x = addProcMatcher x procRegex False cwdRegex

            -- wrap logCallback in a PidCache to filter out PROC_FAILURE
            -- events we don't have information on
            -- (i.e. PROC_FAILURE events where we can't find the program
            -- name and therefore don't care about)
            logCallback' :: IORef PidCache -> DbMonad (EventCallback)
            logCallback' ref = callbackAsIO (logCallback logError) >>= liftIO . return . withPidCache ref

        in (liftIO initPidCache >>= logCallback') >>= \c ->
            liftIO $ do
                let programLoggerAction = procM c >> listenForever
                res <- runProgramLogger programLoggerAction
                putStrLn $ "res = " ++ (show res)
