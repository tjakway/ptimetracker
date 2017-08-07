{-# LANGUAGE ExistentialQuantification #-}
module TimeTracker.IO.Database where

import qualified TimeTracker.Config.ConfigTypes as TimeTracker
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

createTablesStmt' :: IConnection a => a -> IO Statement
createTablesStmt' a = prepare a "CREATE TABLE ProcEventTypes( \
                                \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                                \ name TEXT)"

insertProcEventTypeStmt

mkDbData :: TimeTracker.Config -> IO DbData
mkDbData conf = do
        conn <- connect (connectionInfo conf)
    where connect (Sqlite path) = connectSqlite3 path
