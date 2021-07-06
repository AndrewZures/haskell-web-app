{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy
import Lib
import Model.Image
import Web.Scotty

getString :: [String]
getString = ["howdy", "hardy", "hola"]

main =
  scotty 3000 $ do
    post "/images" $ do
      createImageParams <- jsonData :: ActionM CreateImageParams
      let updatedImageParams = updateDetectedObjects getString createImageParams
      maybeImage <- liftIO $ createImage updatedImageParams
      case maybeImage of
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
