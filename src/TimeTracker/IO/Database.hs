{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables,
FlexibleContexts #-}
module TimeTracker.IO.Database 
(
runDbMonad
)
where

import TimeTracker.Interface (ProcEventData, procEventDataToInt)
import qualified TimeTracker.Config.ConfigTypes as TimeTracker
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time.Clock
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

-- requires Rank2Types
type StatementFunction = forall a . IConnection a => a -> IO Statement

runDbMonad :: DbMonad a -> DbData -> IO a
runDbMonad s r = runReaderT s' r
    where s' = setupDbMonad >> s >>= \x -> (cleanupDbMonad >> return x)



setupDbMonad :: DbMonad ()
setupDbMonad = setupBeforeTables >> createTables

    where   setupBeforeTables :: DbMonad ()
            setupBeforeTables = do
                    DbData { connection = c }  <- ask
                    -- enable foreign keys if we're using SQLite
                    let fkPragma = runRaw c "PRAGMA foreign_keys = ON;"
                    cI <- connInfo <$> ask
                    liftIO . when (TimeTracker.isSqlite cI) $ fkPragma


            createTables :: DbMonad ()
            createTables = (createTablesStmt <$> ask) >>= liftIO . executeRaw



createTablesStmt' :: StatementFunction
createTablesStmt' = flip prepare $ "CREATE TABLE IF NOT EXISTS ProcEventTypes( \
                                    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                                    \ name TEXT); \
                                \ CREATE TABLE IF NOT EXISTS ProcEvents( \
                                    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                                    \ eventType INTEGER FOREIGN KEY REFERENCES ProcEventTypes(id), \
                                    \ when DATETIME, \
                                    \ programName TEXT, \
                                    \ path TEXT); \
                                \ CREATE TABLE IF NOT EXISTS TickResolutions( \
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


-- TODO: instead of returning Integers, have a better way to check errors

insertProcEventType :: String -> DbMonad Integer
insertProcEventType s =
        (insertProcEventTypeStmt <$> ask) >>= (liftIO . (\stmt -> execute stmt s'))
        where s' :: [SqlValue]
              s' = return . toSql $ s

insertProcEvents :: [(ProcEventData, UTCTime, String, String)] -> DbMonad ()
insertProcEvents xs = (insertProcEventStmt <$> ask) >>= \stmt -> (liftIO $ executeMany stmt xs')
    where conv (a, b, c, d) = [toSql . procEventDataToInt $ a, toSql b, toSql c, toSql d]
          xs' = map conv xs

-- | the ID is a foreign key into ProcEventType
insertTickResolution :: Integer -> Integer -> DbMonad Integer
insertTickResolution procEventTypeId resolutionMillis = 
        (insertTickResolutionStmt <$> ask) >>= (liftIO . (\stmt -> execute stmt values))
    where values = [toSql procEventTypeId, toSql resolutionMillis]

-- possibly use StateT on IO to pass the connection?
mkDbData :: TimeTracker.Config -> IO DbData
mkDbData conf = do
        let cI                  =  TimeTracker.connectionInfo conf
        c                       <- (connect (TimeTracker.connectionInfo conf)) :: IO Connection
        cTablesStmt             <- createTablesStmt' c
        insProcEventTypeStmt    <- insertProcEventTypeStmt' c
        -- XXX: other statements
        insProcEventStmt        <- insertProcEventStmt' c
        insTickResolutionStmt   <- insertTickResolutionStmt' c

        return $ DbData { connection = c,
                          connInfo   = cI,
                          createTablesStmt = cTablesStmt,
                          insertProcEventTypeStmt = insProcEventTypeStmt,
                          insertProcEventStmt = insProcEventStmt,
                          insertTickResolutionStmt = insTickResolutionStmt
                          }
    where connect (TimeTracker.Sqlite path) = connectSqlite3 path -- TODO: postgres

cleanupDbMonad :: DbMonad ()
cleanupDbMonad = undefined -- XXX
