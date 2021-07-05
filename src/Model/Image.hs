{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Aeson
import Data.Maybe
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
import GHC.Generics

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Image json
    label String
    mime String
    src String
    detectionEnabled Bool default=True
    createdAt UTCTime Maybe default=CURRENT_TIMESTAMP
    updatedAt UTCTime Maybe default=CURRENT_TIMESTAMP
    deriving Show
    |]

data ImageData = ImageData {mime :: String, src :: String}
  deriving (Show, Generic, ToJSON, FromJSON)

data CreateImageParams = CreateImageParams
  { label :: String,
    image :: ImageData,
    detectionEnabled :: Maybe Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

paramsToImage :: CreateImageParams -> Image
paramsToImage (CreateImageParams label img detectionEnabled) = do
  Image label (mime img) (src img) (fromMaybe True detectionEnabled) Nothing Nothing

createImage :: CreateImageParams -> IO (Maybe Image)
createImage params = runDBIO $ do
  key <- insert $ paramsToImage params
  get key

-- main :: IO ()
-- main = runDBIO $ runMigration migrateAll
