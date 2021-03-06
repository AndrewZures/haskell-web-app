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
import Data.Text.Internal
import Data.Time
import Data.UUID
import Database.Connection (runDBIO)
import Database.Esqueleto.PostgreSQL.JSON hiding ((?&.))
import Database.Persist
  ( Entity (Entity),
    PersistStoreRead (get),
    PersistStoreWrite (insert),
    selectFirst,
    selectList,
    (==.),
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
import Util (newUUID)

data CreateImageParams = CreateImageParams
  { label :: Maybe String,
    uri :: String,
    detectionEnabled :: Maybe Bool,
    detectedObjects :: Maybe [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Image json
    uuid String
    label String
    uri String
    detectionEnabled Bool default=True
    detectedObjects [Text] sqltype=jsonb
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show
    |]

convertToImage :: CreateImageParams -> IO Image
convertToImage params = do
  uuid <- liftIO newUUID
  paramsToImage params uuid

paramsToImage :: CreateImageParams -> UUID -> IO Image
paramsToImage params uuid = do
  time <- getCurrentTime
  return $ Image uuid' label' uri' detectionEnabled' detectedObjects' time time
  where
    uuid' = toString uuid
    label' = fromMaybe uuid' (label params)
    uri' = uri params
    detectionEnabled' = Just False /= detectionEnabled params
    detectedObjects' = fromMaybe [] (detectedObjects params)

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
  -- TODO: convert objects list into jsonb query
  runDBIO $ selectList [] []
