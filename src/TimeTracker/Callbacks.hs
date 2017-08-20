module TimeTracker.Callbacks where

-- TODO: remove unused imports
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import TimeTracker.Interface
import TimeTracker.Types
import TimeTracker.IO.Database
import TimeTracker.IO.Ticks
import TimeTracker.PidCache
import qualified TimeTracker.FFI as FFI
import System.Exit


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


logCallback :: Int -> Int -> String -> DbMonad ()
logCallback pid procEventTypeInt progName = 
        let procEventData = FFI.intToProcMatchEventType (fromIntegral procEventTypeInt) >>= 
                   \x -> case x of FFI.Other     -> Just Other
                                   FFI.NoEvent   -> Just NoEvent
                                   FFI.ProcStart -> Just . ProcStart . fromIntegral $ pid
                                   FFI.ProcEnd   -> Just . ProcEnd . fromInteger $ -1 -- TODO: need to get the actual exit code
        in do
            -- TODO: fix line length
            case procEventData of Nothing -> logError ("Received unknown event type from progName" ++ progName)
                                  Just ev -> insertProcEvents [(ev, progName)] >> commitDb

dbMonadAction :: Int -> DbMonad ExitCode
dbMonadAction tickResolution = 
        let procRegex = ".*"
            cwdRegex = ".*"
            procM x = addProcMatcher x procRegex False cwdRegex

            -- wrap logCallback in a PidCache to filter out PROC_FAILURE
            -- events we don't have information on
            -- (i.e. PROC_FAILURE events where we can't find the program
            -- name and therefore don't care about)
            logCallback' :: IORef PidCache -> DbMonad (EventCallback)
            logCallback' ref = callbackAsIO logCallback >>= liftIO . return . withPidCache ref

        in do
            cacheRef <- liftIO initPidCache 
            callback <- logCallback' cacheRef

            -- | it's OK if we launch the tick recording thread early
            -- because if the PID cache is empty then startRecordingTicks will
            -- have no effect
            -- (PidCache updates are atomic)
            maybeThreadId <- startRecordingTicks cacheRef tickResolution 

            let logTickError = logError "Error while launching tick recording thread"

            case maybeThreadId of Nothing -> logTickError >> return (ExitFailure (-1))
                                  Just threadId -> do
                                    let programLoggerAction = procM callback >> listenForever
                                    res <- liftIO . runProgramLogger $ programLoggerAction
                                    return $ if res == 0 
                                                 then ExitSuccess 
                                                 else ExitFailure (fromIntegral res)
