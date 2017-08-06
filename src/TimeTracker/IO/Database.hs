{-# LANGUAGE ExistentialQuantification #-}
module TimeTracker.IO.Database where

import Database.HDBC
import Control.Monad.Reader

data DBData = 
    forall a . IConnection a => 
        DBData {
            connection :: a,
            createTablesStmt :: Statement,
            insertProcEventStmt :: Statement,
            insertTickResolutionStmt :: Statement,
            insertProcEventTypeStmt :: Statement
        }

type DBMonad a = ReaderT DBData IO a

createTables :: DBMonad ()
createTables = (createTablesStmt <$> ask) >>= liftIO . executeRaw
