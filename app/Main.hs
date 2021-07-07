{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy
import Lib
import Model.Image
import Service.Image
import Web.Scotty
  ( ActionM,
    get,
    html,
    json,
    jsonData,
    param,
    post,
    scotty,
  )

getString :: [String]
getString = ["howdy", "hardy", "hola"]

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
      images <- liftIO getImages
      json images

    get "/images/:uuid" $ do
      imageUUID <- param "uuid"
      maybeImage <- liftIO $ getImage imageUUID
      case maybeImage of
        Nothing -> html "something went wrong"
        Just image -> json image
