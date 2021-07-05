{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Image where

import Control.Monad.IO.Class (liftIO)
import Data.Time
import Database.Connection (runDBIO)
import Database.Persist
  ( PersistStoreRead (get),
    PersistStoreWrite (insert),
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Image
    label String
    detectionEnabled Bool default=True
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show
    |]

createImage :: String -> Bool -> IO (Maybe Image)
createImage name detectionEnabled = runDBIO $ do
  time <- liftIO getCurrentTime
  key <- insert $ Image name detectionEnabled time time
  get key

-- main :: IO ()
-- main = runDBIO $ runMigration migrateAll
