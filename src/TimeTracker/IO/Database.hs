{-# LANGUAGE ExistentialQuantification, Rank2Types, ScopedTypeVariables,
FlexibleContexts #-}
module TimeTracker.IO.Database 
(
DbMonad,
mkDbData,
runDbMonad,
insertProcEventType,
insertProcEvents,
insertTickResolution,
commitDb,
callbackAsIO
)
where

import TimeTracker.Interface (ProcEventData(..), procEventDataToInt, EventCallback)
import qualified TimeTracker.Config.ConfigTypes as TimeTracker
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Safe
import Control.Monad (liftM3)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception (bracketOnError)

data DbData = 
    forall a . IConnection a => 
        DbData {
            connection :: a,
            connInfo :: TimeTracker.ConnectionInfo,

            insertProcEventTypeStmt :: Statement,
            insertProcEventStmt :: Statement,
            insertTickResolutionStmt :: Statement,

            selectProcEventTypeStmt :: Statement,
            selectAllProcEventTypesStmt :: Statement
        }

type DbMonad a = ReaderT DbData IO a

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

-- | for use within this module (not exported)
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
                    mapM_ insertProcEventType startingProcEventNames
                    commitDb

        
data ProcEventType = ProcEventType {
                    procEventTypeName :: String,
                    procEventTypeId   :: Int
                   }
                   deriving (Eq)

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

selectAllProcEventTypesStmt' :: StatementFunction
selectAllProcEventTypesStmt' = sprepare "SELECT * FROM ProcEventTypes"

-- TODO: instead of returning Integers, have a better way to check errors

insertProcEventType :: String -> DbMonad Integer
insertProcEventType s =
        (insertProcEventTypeStmt <$> ask) >>= (liftIO . (\stmt -> execute stmt s'))
        where s' :: [SqlValue]
              s' = return . toSql $ s

insertProcEvents :: [(ProcEventData, String)] -> DbMonad ()
insertProcEvents xs = (insertProcEventStmt <$> ask) >>= \stmt -> (liftIO $ executeMany stmt xs')
    where conv (a, b) = [toSql . procEventDataToInt $ a, toSql b]
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
                            liftIO . \s -> (execute s name' >> 
                                           fetchRow s >>= 
                                           return . fmap fromSql . (headMay =<<))
                where name' = [toSql name]

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
            c                       <- get

            return $ DbData { connection = c,
                            connInfo   = cI,
                            insertProcEventTypeStmt = insProcEventTypeStmt,
                            insertProcEventStmt = insProcEventStmt,
                            insertTickResolutionStmt = insTickResolutionStmt,
                            selectProcEventTypeStmt  = selProcEventTypeStmt,
                            selectAllProcEventTypesStmt  = selAllProcEventTypesStmt
                            }


-- TODO
cleanupDbMonad :: DbMonad ()
cleanupDbMonad = do
        DbData { connection = c } <- ask
        liftIO . commit $ c
        liftIO . disconnect $ c

callbackAsIO :: (Integer -> Integer -> String -> DbMonad ()) -> DbMonad EventCallback
callbackAsIO callback = do
        dbData <- ask 
        return $ \a b c -> runDbMonadWithState (callback a b c) dbData


--x :: Monad m =>
--     m (IO a) -> m (a -> IO b) -> m (a -> IO c) -> m (IO c)


bracketOnErrorM_ :: DbMonad a -> DbMonad b -> DbMonad c -> DbMonad c
bracketOnErrorM_ a b c = (liftM3 bracketOnError) a' b' c' >>= liftIO
    where 
          run' :: DbMonad a -> DbMonad (IO a)
          run' x = runDbMonadWithState x <$> ask

          run2' :: DbMonad b -> DbMonad (a -> IO b)
          run2' f = do
              s <- ask
              return $ \_ -> runDbMonadWithState f s

          a'    = run' a
          b'    = run2' b
          c'    = run2' c
