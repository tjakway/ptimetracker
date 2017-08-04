module TimeTracker.IO.Queries where

import Database.HDBC

insertIfDoesntContain = undefined

createTables :: (IConnection a) => a -> IO ()
createTables = undefined
    where createEventTable = "CREATE TABLE Events()"
