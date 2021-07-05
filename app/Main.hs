{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Text.Lazy
import Lib
import Model.Image
import Web.Scotty

main =
  scotty 3000 $
    get "/images" $ do
      name <- param "name"
      maybeImage <- liftIO $ createImage name
      case maybeImage of
        Nothing -> html "something went wrong"
        Just (Image id) -> html $ mconcat ["<h1>Scotty, ", pack id, " me up!</h1>"]
