{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Image where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Base (Any)
import GHC.Generics (Generic)
import Model.Image
import Network.HTTP.Simple

newtype ImagaTagDetails = ImagaTagDetails
  { en :: T.Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''ImagaTagDetails

data ImagaTag = ImagaTag
  { confidence :: Float,
    tag :: ImagaTagDetails
  }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''ImagaTag

newtype ImagaResults = ImagaResult
  { tags :: [ImagaTag]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''ImagaResults

newtype ImagaTagResponse = ImagaTagResponse
  { result :: ImagaResults
  }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''ImagaTagResponse

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

fetchAndAttachDetectedObjects :: CreateImageParams -> IO CreateImageParams
fetchAndAttachDetectedObjects params = do
  response <- fetchDetectObjects (uri params)
  return params {detectedObjects = detectedObjects response}
  where
    detectedObjects response = parseDetectedObjectsFromResponse $ getResponseBody response

parseDetectedObjectsFromResponse :: ImagaTagResponse -> [T.Text]
parseDetectedObjectsFromResponse response =
  map tagStr itags
  where
    iresults = result response
    itags = tags iresults
    tagStr itag = en $ tag itag

fetchDetectObjects :: String -> IO (Response ImagaTagResponse)
fetchDetectObjects uri = do
  let request =
        setRequestQueryString [("image_url", Just convertedUri)] $
          setRequestHeader
            "Content-Type"
            ["application/json"]
            $ setRequestHeader
              "Authorization"
              ["Basic YWNjX2UyNjFjNDRkZjM5YTkxZDpjNTg3Njg1OWExNmY3NTVlZmU3ZTlmNjAyNDI3NzkxNA=="]
              "GET https://api.imagga.com/v2/tags"
  httpJSON request :: IO (Response ImagaTagResponse)
  where
    convertedUri = packStr'' uri
