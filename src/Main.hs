{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import System.Exit
import qualified System.Log.Logger as Logger
import Control.Monad (when)
import System.Posix.User
import TimeTracker.IO.Database
import TimeTracker.Config.ConfigTypes
import TimeTracker.PidCache
import TimeTracker.Callbacks

logDebug :: String -> IO ()
logDebug = Logger.debugM "Main"

logError :: String -> IO ()
logError = Logger.errorM "Main"

exitIfNotRoot :: IO ()
exitIfNotRoot = do
        isRoot <- fmap (== 0) getRealUserID
        when (not isRoot) $ do
            hPutStrLn stderr "Must be run as root."
            exitFailure


main :: IO ()
main = do
        exitIfNotRoot
        putStrLn "Haskell main started."
        let conf = Config {connectionInfo = Sqlite "test.db", ticksEnabled = True }
        runDbMonad conf (dbMonadAction logError)
