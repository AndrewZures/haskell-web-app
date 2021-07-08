module Service.Image where

import Client.Imagga
import Data.Text (Text)
import Model.Image

fetchAndAttachDetectedObjects :: CreateImageParams -> IO CreateImageParams
fetchAndAttachDetectedObjects params =
  if detectionEnabled' then fetchAndAttachDetectedObjects' params else return params
  where
    detectionEnabled' = Just False /= detectionEnabled params

fetchAndAttachDetectedObjects' :: CreateImageParams -> IO CreateImageParams
fetchAndAttachDetectedObjects' params = do
  response <- fetchDetectObjects (uri params)
  return params {detectedObjects = parseDetectedObjectNames response}
  where
    parseDetectedObjectNames objects = parseDetectedObjectsFromResponse objects

parseDetectedObjectsFromResponse :: ImagaTagResponse -> Maybe [Text]
parseDetectedObjectsFromResponse response =
  return $ map tagStr itags
  where
    iresults = result response
    itags = tags iresults
    tagStr itag = en $ tag itag
