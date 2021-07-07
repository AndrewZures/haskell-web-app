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

module Model.Image where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe
import Data.Text.Internal
import Data.Time
import Data.UUID
import Database.Connection (runDBIO)
import Database.Esqueleto.PostgreSQL.JSON hiding ((?&.))
import Database.Persist
  ( Entity (Entity),
    Filter (Filter),
    PersistStoreRead (get),
    PersistStoreWrite (insert),
    selectFirst,
    selectList,
    toPersistValueJSON,
    (<-.),
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
    uri :: String,
    detectionEnabled :: Maybe Bool,
    detectedObjects :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newUUID :: IO UUID
newUUID = randomIO

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Image json
    uuid String
    label String
    mime String
    uri String
    detectionEnabled Bool default=True
    detectedObjects [Text] sqltype=jsonb
    createdAt UTCTime Maybe default=CURRENT_TIMESTAMP
    updatedAt UTCTime Maybe default=CURRENT_TIMESTAMP
    deriving Show
    |]
makeLenses ''Image

convertToImage :: CreateImageParams -> IO Image
convertToImage params = do
  uuid <- liftIO newUUID
  return $ paramsToImage params uuid

paramsToImage :: CreateImageParams -> UUID -> Image
paramsToImage params uuid =
  Image uuid' label' mime' uri' detectionEnabled' detectedObjects' Nothing Nothing
  where
    uuid' = toString uuid
    label' = label params
    mime' = mime params
    uri' = uri params
    detectionEnabled' = Just False /= detectionEnabled params
    detectedObjects' = detectedObjects params

saveImage :: Image -> IO (Maybe Image)
saveImage image = runDBIO $ do
  key <- insert image
  get key

getImage :: String -> IO (Maybe (Entity Image))
getImage imageUUID =
  runDBIO $
    selectFirst [ImageUuid ==. imageUUID] []

getImages :: [Text] -> IO [Entity Image]
getImages objects = do
  runDBIO $ selectList [] []

-- main :: IO ()
-- main = runDBIO $ runMigration migrateAll
