{-# LANGUAGE ExistentialQuantification #-}
module TimeTracker.IO.Database where

import Database.HDBC
import Control.Monad.Reader

data DbData = 
    forall a . IConnection a => 
        DBData {
            connection :: a,
            createTablesStmt :: Statement,
            insertProcEventStmt :: Statement,
            insertTickResolutionStmt :: Statement,
            insertProcEventTypeStmt :: Statement
        }

type DbMonad a = ReaderT DbData IO a

createTables :: DbMonad ()
createTables = (createTablesStmt <$> ask) >>= liftIO . executeRaw
