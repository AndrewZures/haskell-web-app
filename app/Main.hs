{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy
import Lib
import Models (Image (Image), create)
import Web.Scotty

main = scotty 3000 $
  get "/images" $ do
    maybeImage <- liftIO createImage
    case maybeImage of
      Nothing -> html "something went wrong"
      Just (Image key) -> html $ mconcat ["<h1>Scotty, ", pack key, " me up!</h1>"]

createImage = create $ Image "first image"
