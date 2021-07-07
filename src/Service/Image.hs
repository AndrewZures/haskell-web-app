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

data Another = Another
  { en :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''Another

data ImagaTag = ImagaTag
  { confidence :: Float,
    tag :: Another
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
  response <- fetchStuff (uri params)
  let detectedObjs = detectedObjects response
  putStrLn $ "detected objects: " ++ show detectedObjs
  return params {detectedObjects = detectedObjs}
  where
    detectedObjects response = detectedObjectsFromResponse $ getResponseBody response

detectedObjectsFromResponse :: ImagaTagResponse -> [String]
detectedObjectsFromResponse response =
  map tagEn tagsF
  where
    resultsF = result response
    tagsF = tags resultsF
    tagEn itag = en $ tag itag

fetchStuff :: String -> IO (Response ImagaTagResponse)
fetchStuff uri = do
  let request =
        setRequestQueryString [("image_url", Just convertedUri)] $
          setRequestHeader
            "Content-Type"
            ["application/json"]
            $ setRequestHeader
              "Authorization"
              ["Basic YWNjX2UyNjFjNDRkZjM5YTkxZDpjNTg3Njg1OWExNmY3NTVlZmU3ZTlmNjAyNDI3NzkxNA=="]
              "GET https://api.imagga.com/v2/tags"
  response <- httpJSON request :: IO (Response ImagaTagResponse)
  putStrLn $
    "The status code was: "
      ++ show (getResponseStatusCode response)
  putStrLn $
    "The body was: "
      ++ show (getResponseBody response)
  return response
  where
    convertedUri = packStr'' uri

-- "GET https://api.imagga.com"
-- response <- httpJSON request
-- putStrLn $
--   "The status code was: "
--     ++ show (getResponseStatusCode response)

-- setRequestHeader "Authorization: Basic YWNjX2UyNjFjNDRkZjM5YTkxZDpjNTg3Njg1OWExNmY3NTVlZmU3ZTlmNjAyNDI3NzkxNA==" $
--   "GET https://api.imagga.com"

-- httpJSON request
