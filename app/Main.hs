{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack, splitOn, toStrict)
import Model.Image (CreateImageParams, convertToImage, getImage, getImages, saveImage)
import Service.Image (fetchAndAttachDetectedObjects)
import Web.Scotty
  ( ActionM,
    get,
    html,
    json,
    jsonData,
    param,
    post,
    rescue,
    scotty,
  )

main =
  scotty 3000 $ do
    post "/images" $ do
      createImageParams <- jsonData :: ActionM CreateImageParams
      updatedParams <- liftIO $ fetchAndAttachDetectedObjects createImageParams
      image <- liftIO $ convertToImage updatedParams
      savedImage <- liftIO $ saveImage image
      case savedImage of
        Nothing -> html "something went wrong"
        Just image -> json image

    get "/images" $ do
      name <- param "objects" `rescue` (\_ -> return $ pack "")
      images <- liftIO $ getImages $ map toStrict $ splitOn (pack ",") name
      json images

    get "/images/:uuid" $ do
      imageUUID <- param "uuid"
      maybeImage <- liftIO $ getImage imageUUID
      case maybeImage of
        Nothing -> html "something went wrong"
        Just image -> json image
