module TimeTracker.ProcEventType where

data ProcEventType = ProcEventType {
                    procEventTypeName :: String,
                    procEventTypeId   :: Int
                   }
                   deriving (Eq)
