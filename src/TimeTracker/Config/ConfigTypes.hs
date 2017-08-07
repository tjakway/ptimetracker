module TimeTracker.Config.ConfigTypes where


data ConnectionInfo = Sqlite FilePath
                    --Postgres URL of the form https://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT
                    | Postgres String

data Config = Config {
              connectionInfo :: ConnectionInfo
              ticksEnabled :: Bool
            }

