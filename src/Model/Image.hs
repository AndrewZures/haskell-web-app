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

module Model.Image (createImage, getImages, getImage, updateDetectedObjects, CreateImageParams) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe
import Data.Time
import Data.UUID
import Database.Connection (runDBIO)
import Database.Esqueleto.PostgreSQL.JSON
import Database.Persist
  ( Entity (Entity),
    PersistQueryRead (selectFirst),
    PersistStoreRead (get),
    PersistStoreWrite (insert),
    selectList,
    (==.),
    (>=.),
  )
import Database.Persist.Postgresql.JSON
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics
import System.Random

data CreateImageParams = CreateImageParams
  { label :: String,
    mime :: String,
    src :: String,
    detectionEnabled :: Maybe Bool,
    detectedObjects :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

updateDetectedObjects :: [String] -> CreateImageParams -> CreateImageParams
updateDetectedObjects objects params = params {detectedObjects = objects}

newUUID :: IO UUID
newUUID = randomIO

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Image json
    uuid String
    label String
    mime String
    src String
    detectionEnabled Bool default=True
    detectedObjects (JSONB [String])
    createdAt UTCTime Maybe default=CURRENT_TIMESTAMP
    updatedAt UTCTime Maybe default=CURRENT_TIMESTAMP
    deriving Show
    |]

paramsToImage :: CreateImageParams -> UUID -> Image
paramsToImage params uuid =
  Image uuidStr label' mime' src' detectionEnabled' detectedObjects' Nothing Nothing
  where
    label' = label params
    mime' = mime params
    src' = src params
    detectedObjects' = JSONB (detectedObjects params)
    detectionEnabled' = Just False /= detectionEnabled params
    uuidStr = toString uuid

createImage :: CreateImageParams -> IO (Maybe Image)
createImage params = runDBIO $ do
  uuid <- liftIO newUUID
  key <- insert $ paramsToImage params uuid
  get key

getImage :: String -> IO (Maybe (Entity Image))
getImage imageUUID =
  runDBIO $
    selectFirst [ImageUuid ==. imageUUID] []

getImages :: IO [Entity Image]
getImages =
  runDBIO $
    selectList [ImageLabel ==. "json-image2"] []

-- main :: IO ()
-- main = runDBIO $ runMigration migrateAll
