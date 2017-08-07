module TimeTracker.Config where

import TimeTracker.Config.ConfigTypes
import System.Console.GetOpt

data Flag = EnableTicks
          | DbUrl String
          | DbUsername String
          | DbPassword String
    deriving (Show, Eq)


-- XXX: TODO
-- TODO: write configuration defaults
flagsToConfig :: [Flag] -> Either String Config
flagsToConfig [] = Left "No flags passed."

options :: [OptDescr Flag]
options = 
        [ Option ['t'] ["enable-ticks"] (NoArg EnableTicks) "Enable periodic program logging.",
          Option [] ["db-url"] (ReqArg DbUrl "URL") "URL or filesystem path of the database to connect to.",
          Option ['u'] ["username"] (ReqArg DbUsername "USERNAME") "Username for the database, if applicable.",
          Option ['p'] ["password"] (ReqArg DbPassword "PASSWORD") "Password for the database, if applicable."
            ]



