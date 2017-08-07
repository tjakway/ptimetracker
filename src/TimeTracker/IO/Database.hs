{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module TimeTracker.IO.Database where

import qualified TimeTracker.Config.ConfigTypes as TimeTracker
import Database.HDBC
import Control.Monad.Reader

data DbData = 
    forall a . IConnection a => 
        DbData {
            connection :: a,
            connInfo :: TimeTracker.ConnectionInfo,

            createTablesStmt :: Statement,
            insertProcEventTypeStmt :: Statement,
            insertProcEventStmt :: Statement,
            insertTickResolutionStmt :: Statement
        }

type DbMonad a = ReaderT DbData IO a

type StatementFunction = forall a . IConnection a => a -> IO Statement

setupDbMonad :: DbMonad()
setupDbMonad = setupBeforeTables >> createTables

    where   setupBeforeTables :: DbMonad ()
            setupBeforeTables = do
                    c  <- connection <$> get
                    -- enable foreign keys if we're using SQLite
                    let fkPragma = runRaw c "PRAGMA foreign_keys = ON;"
                    cI <- connInfo <$> get
                    when (cI == Sqlite) fkPragma


            createTables :: DbMonad ()
            createTables = (createTablesStmt <$> ask) >>= liftIO . executeRaw



createTablesStmt' :: StatementFunction
createTablesStmt' = flip prepare $ "CREATE TABLE ProcEventTypes( \
                                    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                                    \ name TEXT); \
                                \ CREATE TABLE ProcEvents( \
                                    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                                    \ eventType INTEGER FOREIGN KEY REFERENCES ProcEventTypes(id), \
                                    \ when DATETIME, \
                                    \ programName TEXT, \
                                    \ path TEXT); \
                                \ CREATE TABLE TickResolutions( \
                                    \ id INTEGER FOREIGN KEY REFERENCES ProcEventTypes(id), \
                                    \ resolutionMillis INTEGER NOT NULL);"
                                    -- TODO: should TickResolutions use
                                    -- a composite primary key of 
                                    -- (id, resolutionMillis)?

insertProcEventTypeStmt' :: StatementFunction
insertProcEventTypeStmt' = flip prepare $ "INSERT INTO ProcEventTypes(name) VALUES (?)"

insertProcEventStmt' :: StatementFunction
insertProcEventStmt' = flip prepare $ "INSERT INTO ProcEvents(eventType, when, programName, path) VALUES (?, ?, ?, ?)"

insertTickResolutionStmt' :: StatementFunction
insertTickResolutionStmt' = flip prepare $ "INSERT INTO TickResolutions(id, resolutionMillis) VALUES (?, ?)"


-- possibly use StateT on IO to pass the connection?
mkDbData :: TimeTracker.Config -> IO DbData
mkDbData conf = do
        let cI                  =  TimeTracker.connectionInfo conf
        c                       <- connect (TimeTracker.connectionInfo conf)
        createTablesStmt        <- createTablesStmt' c
        insProcEventTypeStmt    <- insertProcEventTypeStmt' c
        -- XXX: other statements
        insProcEventStmt        <- insertProcEventStmt' c
        insTickResolutionsStmt  <- insertTickResolutionStmt' c

        return $ DbData { connection = c,
                          connInfo   = cI,
                          createTablesStmt = createTablesStmt',
                          insertProcEventTypeStmt = insertProcEventTypeStmt',
                          insertProcEventStmt = insertProcEventStmt',
                          insertTickResolutionStmt = insertTickResolutionStmt'
                          }
    where connect (Sqlite path) = connectSqlite3 path -- TODO: postgres
