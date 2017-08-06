module TimeTracker.Config where

import System.Console.GetOpt

data Config = Config {
              ticksEnabled :: Bool
            , dbUrl :: String
            , dbUsername :: String
            , dbPassword :: String
            }

data Flag = EnableTicks
          | DbUrl String
          | DbUsername String
          | DbPassword String
    deriving (Show, Eq)


-- XXX: TODO
-- TODO: write configuration defaults
flagsToConfig :: [Flag] -> Either String Config
flags [] = Left "No flags passed."

options :: [OptDescr Flag]
options = 
        [ Option ['t'] ["enable-ticks"] 
            (NoArg EnableTicks) "Enable periodic program logging.",
          Option [] ["db-url"]
            (ReqArg DbUrl "URL or filesystem path of the database to connect to."),
          Option ['u'] ["username"]
            (ReqArg DbUsername) "Username for the database, if applicable.",
          Option ['p'] ["password"]
            (ReqArg DbPassword) "Password for the database, if applicable.",
            ]



