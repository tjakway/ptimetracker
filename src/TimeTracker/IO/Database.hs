module TimeTracker.IO.Database where

import Database.HDBC
import Control.Monad.Reader

data DBData = DBData String -- XXX

type DBMonad a = ReaderT DBData IO a

