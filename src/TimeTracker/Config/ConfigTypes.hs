module TimeTracker.Config.ConfigTypes where

import System.Log.Logger

data ConnectionInfo = Sqlite FilePath
                    --Postgres URL of the form https://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT
                    | Postgres String

data Config = Config {
              loggingPriority :: Priority,
              connectionInfo :: ConnectionInfo,
              ticksEnabled :: Bool
            }

isSqlite :: ConnectionInfo -> Bool
isSqlite (Sqlite _) = True
isSqlite _ = False
