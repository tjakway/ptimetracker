{-# LANGUAGE ScopedTypeVariables #-}
module TimeTracker.Config.Options where

import Data.List (intercalate)
import Data.Char (isSpace, toLower)
import TimeTracker.Config.ConfigTypes
import System.Console.GetOpt
import qualified System.Log.Logger as LogLevel

data Flag = EnableTicks
          | DbUrl String
          | DbUsername String
          | DbPassword String
          | LogPriority String
    deriving (Show, Eq)

-- from https://hackage.haskell.org/package/MissingH-1.4.0.1/docs/Data-Either-Utils.html
-- see Attributions
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just x) = Right x
maybeToEither e Nothing  = Left  e

-- | very, *very*, inefficient...
strip :: String -> String
strip = reverse . stripL . reverse . stripL 
    where stripL = fst . foldr stripHalt ("", True)
          -- stop stripping when we hit non-whitespace (so we don't strip the middle
          -- of the string)
          stripHalt c (acc, True) = if isSpace c then (acc, True)
                                                 -- hit a character, stop
                                                 -- stripping
                                                 else (c : acc, False)
          stripHalt c (acc, False) = (c : acc, False)

readLogPriority :: String -> Either String LogLevel.Priority
readLogPriority priorityStr = maybeToEither failedParseMessage . tryVerbose . tryQuiet . readMaybe $ priorityStr
    where 
        failedParseMessage = 
            -- see https://stackoverflow.com/questions/25924399/haskell-obtain-a-list-of-all-enum-values-without-naming-them
            let logCtors :: [LogLevel.Priority]
                logCtors = enumFrom (toEnum 0)

                logCtorsS = intercalate ", " $ 
                        (map (map toLower . show) logCtors) ++ ["q", "quiet", "v", "verbose"]
                in "Could not parse verbosity/log priority.  Please pass one of (case ignored): " ++ logCtorsS

        tryQuiet = recover (tryParse "q" LogLevel.ERROR) .
                recover (tryParse "QUIET" LogLevel.ERROR)

        tryVerbose = recover (tryParse "v" LogLevel.DEBUG) .
                recover (tryParse "VERBOSE" LogLevel.DEBUG)

        eqIgnoreCase q r = (map toLower q) == (map toLower r)

        tryParse :: String -> a -> Maybe a
        tryParse x a = if x `eqIgnoreCase` priorityStr 
                           then Just a 
                           else Nothing

        recover :: Maybe a -> Maybe a -> Maybe a
        recover _   (Just x) = Just x
        recover alt Nothing  = alt

parseLogPriority :: Flag -> Either String LogLevel.Priority
parseLogPriority (LogPriority s) = readLogPriority s
parseLogPriority _ = Left "Flag is not LogPriority"

readMaybe :: Read a => String -> Maybe a
readMaybe = verify . stripWhitespace . reads 
    where stripWhitespace = map (\(a, t) -> (a, strip t))
          verify [(a, "")] = Just a
          verify _ = Nothing

-- XXX: TODO
-- TODO: write configuration defaults
flagsToConfig :: [Flag] -> Either String Config
flagsToConfig [] = Left "No flags passed."
    where parseLogLevel :: [Flag] -> Either String LogLevel.Priority
          parseLogLevel = mkFlagParser parseLogPriority

mkFlagParser :: (Flag -> Either String a) -> ([Flag] -> Either String a)
mkFlagParser p = foldr f (Left "") 
    -- try parsing every flag until we're out or we've succeeded
    where f _ (Right x) = Right x
          f thisFlag (Left _)  = p thisFlag

options :: [OptDescr Flag]
options = 
        [ Option ['t'] ["enable-ticks"] (NoArg EnableTicks) "Enable periodic program logging.",
          Option [] ["db-url"] (ReqArg DbUrl "URL") "URL or filesystem path of the database to connect to.",
          Option ['u'] ["username"] (ReqArg DbUsername "USERNAME") "Username for the database, if applicable.",
          Option ['p'] ["password"] (ReqArg DbPassword "PASSWORD") "Password for the database, if applicable."
            ]




