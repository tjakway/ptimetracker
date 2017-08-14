{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import System.Exit
import Control.Monad (when)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import System.Posix.User
import TimeTracker.Interface
import TimeTracker.Types
import TimeTracker.IO.Database
import qualified TimeTracker.FFI as FFI

--stand-in for a real logging function
logF :: String -> IO ()
logF = putStrLn

exitIfNotRoot :: IO ()
exitIfNotRoot = do
        isRoot <- fmap (== 0) getRealUserID
        when (not isRoot) $ do
            hPutStrLn stderr "Must be run as root."
            exitFailure


main :: IO ()
main = let eventCallback pid _ name = putStrLn ("PID called from Haskell: " ++ (show pid) ++ ", name: " ++ name)
           procRegex = ".*"
           cwdRegex = ".*"
           procM = addProcMatcher eventCallback procRegex False cwdRegex
        in do
            putStrLn "Haskell main started"
            exitFailure


continueCallback :: IO FFI.StopListeningCallback
continueCallback = do
            counter <- newIORef 0
            let continueCallback' = \_ -> do
                    modifyIORef' counter (+1)
                    count <- readIORef counter
                    putStrLn ("listenUntilCallback' called, count is " ++ (show count))
                    if count > 5 then return (boolToCInt False)
                                 else return (boolToCInt True)
            return continueCallback'

logCallback :: Integer -> Integer -> String -> DbMonad ()
logCallback pid procEventTypeInt progName = 
        let procEventData = FFI.intToProcMatchEventType (fromInteger procEventTypeInt) >>= 
                   \x -> case x of FFI.Other     -> Just Other
                                   FFI.NoEvent   -> Just NoEvent
                                   FFI.ProcStart -> Just . ProcStart . fromInteger $ pid
                                   FFI.ProcEnd   -> Just . ProcEnd . fromInteger $ -1 -- TODO: need to get the actual exit code
        in do
            now <- liftIO getCurrentTime
            -- TODO: fix line length
            case procEventData of Nothing -> liftIO $ logF ("Received unknown event type from progName" ++ progName)
                                  Just ev -> insertProcEvents [(ev, now, progName)]


dbMonadAction :: DbMonad ()
dbMonadAction = 
        let procRegex = ".*"
            cwdRegex = ".*"
            procM x = addProcMatcher x procRegex False cwdRegex

            logCallback' :: DbMonad (EventCallback)
            logCallback' = callbackAsIO logCallback

        in logCallback' >>= \c ->
            liftIO $ do
                continueCallback' <- continueCallback
                let programLoggerAction = procM c >> listenUntilCallback continueCallback'
                res <- runProgramLogger programLoggerAction
                putStrLn $ "res = " ++ (show res)
