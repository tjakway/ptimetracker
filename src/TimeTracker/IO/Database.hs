{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables,
FlexibleContexts, DeriveDataTypeable #-}
module TimeTracker.IO.Database 
(
DbMonad,
logWarning,
logError,
mkDbData,
runDbMonad,
runDbMonadWithState,
insertProcEventType,
insertProcEventTypeByName,
insertProcEvents,
insertTickResolution,
insertTickTypeIfNotExists,
selectTickTypeByResolution,
procEventTypeExists,
commitDb,
rollbackDb,
callbackAsIO,
bracketOnErrorM_
)
where

import TimeTracker.ProcEventType
import TimeTracker.Interface (ProcEventData(..), procEventDataToInt, EventCallback)
import qualified TimeTracker.Config.ConfigTypes as TimeTracker
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Typeable
import Data.Convertible.Base (Convertible)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Safe
import Control.Exception
import Control.Monad (liftM3)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception (bracketOnError)
import System.Log.Logger

data DbData = 
    forall a . IConnection a => 
        DbData {
            connection :: a,
            connInfo :: TimeTracker.ConnectionInfo,

            insertProcEventTypeStmt :: Statement,
            insertProcEventStmt :: Statement,
            insertTickResolutionStmt :: Statement,

            selectProcEventTypeStmt :: Statement,
            selectAllProcEventTypesStmt :: Statement,
            selectProcEventTypeStmtById :: Statement
        }

type DbMonad a = ReaderT DbData IO a

loggerName :: String
loggerName = "DbMonad"

logWarning :: String -> DbMonad ()
logWarning = liftIO . warningM loggerName

logError :: String -> DbMonad ()
logError = liftIO . errorM loggerName

-- | Setup all resources needed for the monadic computation then execute it
-- note: this is expensive, don't call often
runDbMonad :: TimeTracker.Config -> DbMonad a -> IO a
runDbMonad config s = do
        let cI =  TimeTracker.connectionInfo config
        conn   <- openConnection cI
        setupTables conn cI
        dbData <- mkDbData conn config 
        runReaderT s' dbData
    where s' = setupDbMonad >> s >>= \x -> (cleanupDbMonad >> return x)

-- convenient for subcomputations in the DbMonad
runDbMonadWithState :: DbMonad a -> DbData -> IO a
runDbMonadWithState = runReaderT

-- XXX: must be kept up to date with TimeTracker.Interface.ProcEventData--
-- is there a better way to do this?
startingProcEventNames :: Map.Map Int String
startingProcEventNames = Map.fromList xs
    where xs = map (\x -> (procEventDataToInt x, show x)) [Other, NoEvent, ProcStart ignoredArg, ProcEnd ignoredArg]
          -- the constructor argument will be ignored, see
          -- procEventDataToInt
          ignoredArg = -1

commitDb :: DbMonad ()
commitDb = do
    DbData { connection = c }  <- ask
    liftIO . commit $ c

rollbackDb :: DbMonad ()
rollbackDb = do
    DbData { connection = c }  <- ask
    liftIO . rollback $ c

quickQueryDb' :: String -> [SqlValue] -> DbMonad [[SqlValue]]
quickQueryDb' stmt vals = do
    DbData { connection = c }  <- ask
    liftIO . quickQuery' c stmt $ vals


openConnection :: TimeTracker.ConnectionInfo -> IO Connection
openConnection (TimeTracker.Sqlite path) = connectSqlite3 path -- TODO: postgres

setupTables :: IConnection a => a -> TimeTracker.ConnectionInfo -> IO ()
setupTables a cI = setupBeforeTables a cI >> createTables a >> commit a

        where   setupBeforeTables :: IConnection a => a -> TimeTracker.ConnectionInfo -> IO ()
                setupBeforeTables c cI = do
                        -- enable foreign keys if we're using SQLite
                        let fkPragma = runRaw c "PRAGMA foreign_keys = ON;"
                        liftIO . when (TimeTracker.isSqlite cI) $ fkPragma


                createTables :: IConnection a => a -> IO ()
                createTables c = runRaw c createTablesString

setupDbMonad :: DbMonad ()
setupDbMonad =  setupProcEventTypes
    where   
            setupProcEventTypes :: DbMonad ()
            setupProcEventTypes = do
                eventTypes <- Set.fromList . fmap procEventTypeName <$> selectAllProcEventTypes
                when (null eventTypes) $ do
                    mapM_ insertProcEventTypeByName startingProcEventNames
                    commitDb

        

-- requires Rank2Types
type StatementFunction = forall s . IConnection s => StateT s IO Statement

sprepare :: IConnection s => String -> StateT s IO Statement
sprepare sql = get >>= \s -> liftIO . prepare s $ sql


-- \ path TEXT); \ --TODO: add path column to ProcEvents?
-- SQLite can store time as TEXT, REAL, or INTEGER
createTablesString :: String
createTablesString =  "CREATE TABLE IF NOT EXISTS ProcEventTypes( \
                                    \ id INTEGER PRIMARY KEY, \
                                    \ name TEXT NOT NULL); \
                                \ CREATE TABLE IF NOT EXISTS ProcEvents( \
                                    \ id INTEGER PRIMARY KEY, \
                                    \ eventType INTEGER NOT NULL, \
                                    \ evTime TEXT DEFAULT CURRENT_TIMESTAMP, \
                                    \ programName TEXT NOT NULL, \
                                    \ FOREIGN KEY (eventType) REFERENCES ProcEventTypes(id)); \
                                \ CREATE TABLE IF NOT EXISTS TickResolutions( \
                                    \ id INTEGER, \
                                    \ resolutionMillis INTEGER NOT NULL, \
                                    \ FOREIGN KEY(id) REFERENCES ProcEventTypes(id));"
                                    -- TODO: should TickResolutions use
                                    -- a composite primary key of 
                                    -- (id, resolutionMillis)?

insertProcEventTypeStmt' :: StatementFunction
insertProcEventTypeStmt' = sprepare "INSERT INTO ProcEventTypes(name) VALUES (?)"

insertProcEventStmt' :: StatementFunction
insertProcEventStmt' = sprepare "INSERT INTO ProcEvents(eventType, programName) VALUES (?, ?)"

insertTickResolutionStmt' :: StatementFunction
insertTickResolutionStmt' = sprepare "INSERT INTO TickResolutions(id, resolutionMillis) VALUES (?, ?)"

selectProcEventTypeStmt' :: StatementFunction
selectProcEventTypeStmt' = sprepare "SELECT * FROM ProcEventTypes WHERE name=?"

selectProcEventTypeStmtById' :: StatementFunction
selectProcEventTypeStmtById' = sprepare "SELECT * FROM ProcEventTypes WHERE id=?"

selectAllProcEventTypesStmt' :: StatementFunction
selectAllProcEventTypesStmt' = sprepare "SELECT * FROM ProcEventTypes"

-- TODO: instead of returning Integers, have a better way to check errors

-- | inserts a proc event type and returns the generated ID
insertProcEventTypeByName :: String -> DbMonad (Maybe Int)
insertProcEventTypeByName s = do
        stmt <- insertProcEventTypeStmt <$> ask
        _ <- liftIO . execute stmt $ s'
        liftIO $ fmap fromSql . (headMay =<<) <$> fetchRow stmt

        where s' :: [SqlValue]
              s' = return . toSql $ s

insertProcEventType :: ProcEventType -> DbMonad (Maybe Int)
insertProcEventType = insertProcEventTypeByName . procEventTypeName

insertProcEvents :: [(ProcEventData, String)] -> DbMonad ()
insertProcEvents xs = (insertProcEventStmt <$> ask) >>= \stmt -> (liftIO $ executeMany stmt xs')
    where conv (a, b) = [toSql . procEventDataToInt $ a, toSql b]
          xs' = map conv xs

-- | the ID is a foreign key into ProcEventType
insertTickResolution :: Int -> Int -> DbMonad Int
insertTickResolution resolutionMillis procEventTypeId  = 
        (insertTickResolutionStmt <$> ask) >>= (liftIO . fmap fromInteger . (\stmt -> execute stmt values))
    where values = [toSql procEventTypeId, toSql resolutionMillis]

tickEventName :: String
tickEventName = "Tick"

data TickException = TickException String
    deriving (Show, Typeable)

instance Exception TickException


insertTickTypeIfNotExists :: Int -> DbMonad Int
insertTickTypeIfNotExists resolution = do
        tickId <- selectTickTypeByResolution resolution
        case tickId of Just x -> return x
                       Nothing -> do
                              maybeTypeId <- insertProcEventTypeByName tickEventName
                              case maybeTypeId of Just typeId -> insertTickResolution resolution typeId >> return typeId
                                                  Nothing -> liftIO . throwIO . TickException $ "Could not find or insert tick ID"

selectSingleRow :: Convertible SqlValue a => (DbData -> Statement) -> [SqlValue] -> DbMonad (Maybe a)
selectSingleRow sel params = (sel <$> ask) >>= 
                            liftIO . \s -> (execute s params >> 
                                           fetchRow s >>= 
                                           return . fmap fromSql . (headMay =<<))


selectTickTypeByResolution :: Int -> DbMonad (Maybe Int)
selectTickTypeByResolution r =
        fmap fromSql . headMay . join <$> quickQueryDb' queryStr r'
        where r' = [toSql r]
              queryStr = "SELECT ProcEventTypes.id FROM TickResolutions LEFT JOIN ProcEventTypes ON TickResolutions.id=ProcEventTypes.id WHERE TickResolutions.resolutionMillis=?"

-- | search for a ProcEventType by name and return the corresponding ID if
-- it exists
selectProcEventType :: String -> DbMonad (Maybe Int)
selectProcEventType name = (selectProcEventTypeStmt <$> ask) >>= 
                            liftIO . \s -> (execute s name' >> 
                                           fetchRow s >>= 
                                           return . fmap fromSql . (headMay =<<))
                where name' = [toSql name]

--selectProcEventTypeById :: Int -> DbMonad (Maybe String)
--selectProcEventTypeById evId = (selectProcEventTypeById <$> ask) >>=

selectAllProcEventTypes :: DbMonad [ProcEventType]
selectAllProcEventTypes = ((selectAllProcEventTypesStmt <$> ask) >>= \s -> 
        liftIO (execute s [] >>
                fetchAllRows' s)) >>=
        return . map f

    where f xs = let at' c = fromSql . atNote "error retrieving record in selectAllProcEventTypes" c
                     id   = xs `at'` 0
                     name = xs `at'` 1
                     in ProcEventType id name

-- | get whether that ProcEventType exists (search by name)
procEventTypeExists :: String -> DbMonad Bool
procEventTypeExists = fmap isJust . selectProcEventType

-- possibly use StateT on IO to pass the connection?
mkDbData :: IConnection a => a -> TimeTracker.Config -> IO DbData
mkDbData c conf = do
        let cI                  =  TimeTracker.connectionInfo conf
        evalStateT (mkDbData' cI) c

    where 
          mkDbData' :: IConnection s => TimeTracker.ConnectionInfo -> StateT s IO DbData
          mkDbData' cI = do
            insProcEventTypeStmt    <- insertProcEventTypeStmt'
            -- XXX: other statements
            insProcEventStmt        <- insertProcEventStmt'
            insTickResolutionStmt   <- insertTickResolutionStmt'

            selProcEventTypeStmt    <- selectProcEventTypeStmt'
            selAllProcEventTypesStmt<- selectAllProcEventTypesStmt'
            selProcEventTypeById    <- selectProcEventTypeStmtById'
            c                       <- get

            return $ DbData { connection = c,
                            connInfo   = cI,
                            insertProcEventTypeStmt = insProcEventTypeStmt,
                            insertProcEventStmt = insProcEventStmt,
                            insertTickResolutionStmt = insTickResolutionStmt,
                            selectProcEventTypeStmt  = selProcEventTypeStmt,
                            selectAllProcEventTypesStmt  = selAllProcEventTypesStmt,
                            selectProcEventTypeStmtById = selProcEventTypeById
                            }


-- TODO
cleanupDbMonad :: DbMonad ()
cleanupDbMonad = do
        DbData { connection = c } <- ask
        liftIO . commit $ c
        liftIO . disconnect $ c

callbackAsIO :: (Int -> Int -> String -> DbMonad ()) -> DbMonad EventCallback
callbackAsIO callback = do
        dbData <- ask 
        return $ \a b c -> runDbMonadWithState (callback a b c) dbData


bracketOnErrorM_ :: DbMonad a -> DbMonad b -> DbMonad c -> DbMonad c
bracketOnErrorM_ a b c = (liftM3 bracketOnError) a' b' c' >>= liftIO
    where 
          run' :: DbMonad a -> DbMonad (IO a)
          run' x = runDbMonadWithState x <$> ask

          runConst' :: DbMonad b -> DbMonad (a -> IO b)
          runConst' f = do
              s <- ask
              return $ \_ -> runDbMonadWithState f s

          a'    = run' a
          b'    = runConst' b
          c'    = runConst' c
