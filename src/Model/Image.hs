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
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Model.Image (createImage, getImages, CreateImageParams) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe
import Data.Time
import Database.Connection (runDBIO)
import Database.Persist
  ( Entity (Entity),
    PersistStoreRead (get),
    PersistStoreWrite (insert),
    selectList,
    (==.),
    (>=.),
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

data CreateImageParams = CreateImageParams
  { label :: String,
    mime :: String,
    src :: String,
    detectionEnabled :: Maybe Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

paramsToImage :: CreateImageParams -> Image
paramsToImage (CreateImageParams label mime src detectionEnabled) = do
  Image label mime src (fromMaybe True detectionEnabled) Nothing Nothing

createImage :: CreateImageParams -> IO (Maybe Image)
createImage params = runDBIO $ do
  key <- insert $ paramsToImage params
  get key

getImages :: IO [Entity Image]
getImages = runDBIO $ do
  selectList [ImageLabel ==. "json-image2"] []

-- main :: IO ()
-- main = runDBIO $ runMigration migrateAll
