{-# LANGUAGE OverloadedStrings #-}

module Database.Connection (runDBIO) where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT)
import Database.Persist.Postgresql

runDBIO :: ReaderT SqlBackend (LoggingT IO) a -> IO a
runDBIO = runStdoutLoggingT . withPostgresqlConn "dbname=heb" . runSqlConn
