{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import System.Exit
import Control.Monad (when)
import System.Posix.User
import TimeTracker.IO.Database
import TimeTracker.Config.ConfigTypes
import TimeTracker.Callbacks

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
        let conf = Config {connectionInfo = Sqlite "test.db", tickResolutionMillis = 2000 }
            -- TODO: add tick resolution to DbData
        res <- runDbMonad conf (dbMonadAction . tickResolutionMillis $ conf)
        exitWith res
