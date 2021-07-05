{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Lib
import Web.Scotty

main = scotty 3000 $
  get "/images" $ do
    html $ mconcat ["<h1>Scotty, ", "beam", " me up!</h1>"]
