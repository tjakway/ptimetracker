module TimeTracker.IO.Ticks where

import TimeTracker.IO.Database
import TimeTracker.PidCache

-- use bracketOnError from Control.Exception to rollback on exception

-- | eventId = the ID column that 
recordTicks :: PidCache -> Int -> DbMonad ()
recordTicks cache eventId = undefined



