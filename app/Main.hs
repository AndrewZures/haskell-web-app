{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack, splitOn, toStrict)
import Model.Image (CreateImageParams, convertToImage, getImage, getImages, saveImage)
import Network.HTTP.Types.Status (status404, status500)
import Service.Image (fetchAndAttachDetectedObjects)
import Web.Scotty

main =
  scotty 3000 $ do
    post "/images" $ do
      createImageParams <- jsonData :: ActionM CreateImageParams
      updatedParams <- liftIO $ fetchAndAttachDetectedObjects createImageParams
      image <- liftIO $ convertToImage updatedParams
      savedImage <- liftIO $ saveImage image
      case savedImage of
        Nothing -> raiseStatus status500 "Failed to save image"
        Just image -> json image

    get "/images" $ do
      objectsParam <- param "objects" `rescue` (\_ -> return $ pack "")
      let objectsList = map toStrict $ splitOn (pack ",") objectsParam
      images <- liftIO $ getImages objectsList
      json images

    get "/images/:uuid" $ do
      imageUUID <- param "uuid"
      maybeImage <- liftIO $ getImage imageUUID
      case maybeImage of
        Nothing -> raiseStatus status404 "Image not found"
        Just image -> json image
