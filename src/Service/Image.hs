module Service.Image where

import Client.Imaga
import Data.Text (Text)
import Model.Image

fetchAndAttachDetectedObjects :: CreateImageParams -> IO CreateImageParams
fetchAndAttachDetectedObjects params =
  case detectionEnabled' of
    Nothing -> return params
    Just p -> if p then fetchAndAttachDetectedObjects' params else return params
  where
    detectionEnabled' = detectionEnabled params

fetchAndAttachDetectedObjects' :: CreateImageParams -> IO CreateImageParams
fetchAndAttachDetectedObjects' params = do
  response <- fetchDetectObjects (uri params)
  return params {detectedObjects = parseDetectedObjectNames response}
  where
    parseDetectedObjectNames objects = parseDetectedObjectsFromResponse objects

parseDetectedObjectsFromResponse :: ImagaTagResponse -> [Text]
parseDetectedObjectsFromResponse response =
  map tagStr itags
  where
    iresults = result response
    itags = tags iresults
    tagStr itag = en $ tag itag
