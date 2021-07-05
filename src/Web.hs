{-# LANGUAGE OverloadedStrings #-}

module Web where

import Data.Monoid
import Web.Scotty

sample :: ScottyM ()
sample = do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!<h1>"]
