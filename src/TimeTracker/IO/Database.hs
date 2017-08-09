{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables,
FlexibleContexts #-}
module TimeTracker.IO.Database 
(
DbData(..),
mkDbData,
runDbMonad,
insertProcEventType,
insertProcEvents,
insertTickResolution
)
where

import TimeTracker.Interface (ProcEventData, procEventDataToInt)
import qualified TimeTracker.Config.ConfigTypes as TimeTracker
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time.Clock
import Data.Maybe (isJust)
import Safe (headMay)
import Control.Monad.Reader
import Control.Monad.State.Strict

data DbData = 
    forall a . IConnection a => 
        DbData {
            connection :: a,
            connInfo :: TimeTracker.ConnectionInfo,

            createTablesStmt :: Statement,
            insertProcEventTypeStmt :: Statement,
            insertProcEventStmt :: Statement,
            insertTickResolutionStmt :: Statement,

            selectProcEventTypeStmt :: Statement
        }

type DbMonad a = ReaderT DbData IO a

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

-- requires Rank2Types
type StatementFunction = forall s . IConnection s => StateT s IO Statement

sprepare :: IConnection s => String -> StateT s IO Statement
sprepare sql = get >>= \s -> liftIO . prepare s $ sql


createTablesStmt' :: StatementFunction
createTablesStmt' =  sprepare "CREATE TABLE IF NOT EXISTS ProcEventTypes( \
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
insertProcEventTypeStmt' = sprepare "INSERT INTO ProcEventTypes(name) VALUES (?)"

insertProcEventStmt' :: StatementFunction
insertProcEventStmt' = sprepare "INSERT INTO ProcEvents(eventType, when, programName, path) VALUES (?, ?, ?, ?)"

insertTickResolutionStmt' :: StatementFunction
insertTickResolutionStmt' = sprepare "INSERT INTO TickResolutions(id, resolutionMillis) VALUES (?, ?)"

selectProcEventTypeStmt' :: StatementFunction
selectProcEventTypeStmt' = sprepare "SELECT * FROM ProcEventTypes WHERE name=?"



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

-- | search for a ProcEventType by name and return the corresponding ID if
-- it exists
selectProcEventType :: String -> DbMonad (Maybe Integer)
selectProcEventType name = (selectProcEventTypeStmt <$> ask) >>= 
                            liftIO . \s -> (fetchRow s >>= return . fmap fromSql . (headMay =<<))


-- possibly use StateT on IO to pass the connection?
mkDbData :: TimeTracker.Config -> IO DbData
mkDbData conf = do
        let cI                  =  TimeTracker.connectionInfo conf
        c                       <- (connect (TimeTracker.connectionInfo conf)) :: IO Connection

        evalStateT (mkDbData' cI) c

    where connect (TimeTracker.Sqlite path) = connectSqlite3 path -- TODO: postgres

          mkDbData' :: IConnection s => TimeTracker.ConnectionInfo -> StateT s IO DbData
          mkDbData' cI = do
            cTablesStmt             <- createTablesStmt'
            insProcEventTypeStmt    <- insertProcEventTypeStmt'
            -- XXX: other statements
            insProcEventStmt        <- insertProcEventStmt'
            insTickResolutionStmt   <- insertTickResolutionStmt'

            selProcEventTypeStmt    <- selectProcEventTypeStmt'

            c                       <- get

            return $ DbData { connection = c,
                            connInfo   = cI,
                            createTablesStmt = cTablesStmt,
                            insertProcEventTypeStmt = insProcEventTypeStmt,
                            insertProcEventStmt = insProcEventStmt,
                            insertTickResolutionStmt = insTickResolutionStmt,
                            selectProcEventTypeStmt  = selProcEventTypeStmt
                            }


-- TODO
cleanupDbMonad :: DbMonad ()
cleanupDbMonad = do
        DbData { connection = c } <- ask
        liftIO . commit $ c
        liftIO . disconnect $ c
