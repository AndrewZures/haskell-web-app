module Service.Image where

import Control.Monad.IO.Class (liftIO)
import Model.Image (Image (Image))

fetchAndAttachDetectedObjects :: Image -> IO Image
fetchAndAttachDetectedObjects image = do
  return image

-- fetchDetectedObjects :: Image -> IO [String]

-- updateDetectedObjects :: Image -> [String] -> Image
-- updateDetectedObjects image newObjects = image {detectedObjects = JSONB newObjects}
