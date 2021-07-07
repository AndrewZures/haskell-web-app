{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Image where

import Client.Imaga
import Model.Image
import Network.HTTP.Simple

fetchAndAttachDetectedObjects :: CreateImageParams -> IO CreateImageParams
fetchAndAttachDetectedObjects params = do
  response <- fetchDetectObjects (uri params)
  return params {detectedObjects = detectedObjects response}
  where
    detectedObjects response = parseDetectedObjectsFromResponse $ getResponseBody response
