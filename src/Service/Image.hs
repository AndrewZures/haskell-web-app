{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Image where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Base (Any)
import GHC.Generics (Generic)
import Model.Image (Image (..))
import Network.HTTP.Simple

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

fetchAndAttachDetectedObjects :: Image -> IO Image
fetchAndAttachDetectedObjects image = do
  response <- fetchStuff (uri image)
  return image

data ImagaTag = ImagaTag
  { confidence :: Float,
    tag :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ImagaResults = ImagaResult
  { tags :: [ImagaTag]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ImagaTagResponse = ImagaTagResponse
  { results :: ImagaResults
  }
  deriving (Show, Generic, ToJSON, FromJSON)

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
