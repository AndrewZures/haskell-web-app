module Service.Image where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Model.Image (Image (Image))
import qualified Network.AWS.Rekognition.DetectLabels as R
import qualified Network.AWS.Rekognition.Types as T

fetchAndAttachDetectedObjects :: Image -> IO Image
fetchAndAttachDetectedObjects image = do
  return image

-- toUTF8Bytes = Data.ByteString.unpack . encodeUtf8 . Data.Text.pack

-- fetchLabels :: Image -> T.Image
-- fetchLabels (Image _ _ _ src _ _ _ _) =
--   let img = toUTF8Bytes src :: T.Image

-- R.detectLabels rimage

-- fetchImage

-- fetchDetectedObjects :: Image -> IO [String]

-- updateDetectedObjects :: Image -> [String] -> Image
-- updateDetectedObjects image newObjects = image {detectedObjects = JSONB newObjects}
