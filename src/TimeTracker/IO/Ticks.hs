{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module TimeTracker.IO.Ticks where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Typeable
import qualified Data.Map.Strict as Map
import System.Directory
import TimeTracker.ProcEventType
import TimeTracker.IO.Database
import qualified TimeTracker.FFI as FFI
import TimeTracker.PidCache


data TickException = TickException
    deriving (Show, Typeable)

instance Exception TickException

tickEventName :: String
tickEventName = "Tick"

procDirExists :: FFI.PidT -> IO Bool
procDirExists = doesDirectoryExist . procDir
    where procDir :: FFI.PidT -> FilePath
          procDir = mappend "/proc/" . show

-- | check that we have it in the cache and that the /proc/<PID> dir exists
isRunning :: PidCache -> FFI.PidT -> IO Bool
isRunning cache pid = case Map.lookup pid cache of
                          Just _ -> procDirExists pid
                          Nothing -> return False

recordTicks :: ProcEventType -> PidCache -> Int -> DbMonad ()
recordTicks evType cache eventId = bracketOnErrorM_ (return ()) rollbackDb rec'
    where tickEventId = procEventTypeId evType
          rec' = do
              undefined -- XXX


insertTickType :: Int -> DbMonad Int
insertTickType resolution = do
        tickId <- selectTickTypeByResolution resolution
        case tickId of Just x -> return x
                       Nothing -> do
                              maybeTypeId <- insertProcEventTypeByName tickEventName
                              case maybeTypeId of Just typeId -> insertTickResolution resolution typeId >> return typeId
                                                  Nothing -> liftIO . throwIO $ TickException
                                            
