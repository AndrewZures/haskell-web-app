{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy
import Lib
import Model.Image
import Web.Scotty

main =
  scotty 3000 $ do
    post "/images" $ do
      createImageParams <- jsonData :: ActionM CreateImageParams
      maybeImage <- liftIO $ createImage createImageParams
      case maybeImage of
        Nothing -> html "something went wrong"
        Just image -> json image

    get "/images" $ do
      images <- liftIO getImages
      json images
