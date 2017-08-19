{-# LANGUAGE ScopedTypeVariables #-}
module TimeTracker.IO.Ticks where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Typeable
import qualified Data.Map.Strict as Map
import System.Directory
import TimeTracker.ProcEventType
import TimeTracker.IO.Database
import TimeTracker.Interface (ProcEventData(..))
import qualified TimeTracker.FFI as FFI
import TimeTracker.PidCache



procDirExists :: FFI.PidT -> DbMonad Bool
procDirExists = liftIO . doesDirectoryExist . procDir
    where procDir :: FFI.PidT -> FilePath
          procDir = mappend "/proc/" . show

recordTicks :: ProcEventType -> PidCache -> DbMonad ()
recordTicks evType cache = bracketOnErrorM_ (return ()) rollbackDb rec'
    where tickEventId = procEventTypeId evType
          rec' = mapM_ f' . Map.toList $ cache
          f' (pid, progName) = do
              isRunning <- procDirExists pid
              if isRunning then recordTick tickEventId progName
                           else logWarning $ "Program " ++ 
                                    show progName ++ 
                                    " with PID " ++ 
                                    show pid ++ 
                                    " exists in the PidCache " ++
                                    "but its /proc/ folder doesn't exist"




recordTick :: Int -> String -> DbMonad ()
recordTick eventId progName = insertProcEvents [(Tick eventId, progName)]
