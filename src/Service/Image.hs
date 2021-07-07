module Service.Image where

import Client.Imaga
import Data.Text (Text)
import Model.Image (CreateImageParams (detectedObjects, uri))

fetchAndAttachDetectedObjects :: CreateImageParams -> IO CreateImageParams
fetchAndAttachDetectedObjects params = do
  detectedObjects <- fetchDetectObjects (uri params)
  return params {detectedObjects = parseDetectedObjectNames detectedObjects}
  where
    parseDetectedObjectNames objects = parseDetectedObjectsFromResponse objects

parseDetectedObjectsFromResponse :: ImagaTagResponse -> [Text]
parseDetectedObjectsFromResponse response =
  map tagStr itags
  where
    iresults = result response
    itags = tags iresults
    tagStr itag = en $ tag itag
