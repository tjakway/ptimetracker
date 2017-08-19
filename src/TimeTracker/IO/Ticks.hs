{-# LANGUAGE ScopedTypeVariables #-}
module TimeTracker.IO.Ticks where

import System.Directory
import Data.Map.Strict
import TimeTracker.IO.Database
import TimeTracker.PidCache


procDirExists :: PidT -> IO Bool
procDirExists = doesDirectoryExist . procDir
    where procDir :: PidT -> FilePath
          procDir = mappend "/proc/" . show

isRunning :: PidCache -> PidT -> IO Bool
isRunning cache pid = case procDirExists pid <$> lookup pid cache of Just x -> return x
                                                                     Nothing -> return False

recordTicks :: PidCache -> Int -> DbMonad ()
recordTicks cache eventId = bracketOnErrorM_ (return ()) rollbackDb rec'
    where rec' = do

