{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Client.Imaga where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( Response,
    httpJSON,
    setRequestHeader,
    setRequestQueryString,
  )

newtype ImagaTagDetails = ImagaTagDetails
  { en :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ImagaTag = ImagaTag
  { confidence :: Float,
    tag :: ImagaTagDetails
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ImagaResults = ImagaResult
  { tags :: [ImagaTag]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ImagaTagResponse = ImagaTagResponse
  { result :: ImagaResults
  }
  deriving (Show, Generic, ToJSON, FromJSON)

setContentTypeJSON = setRequestHeader "Content-Type" ["application/json"]

setAuthorizationHeader =
  setRequestHeader
    "Authorization"
    ["Basic YWNjX2UyNjFjNDRkZjM5YTkxZDpjNTg3Njg1OWExNmY3NTVlZmU3ZTlmNjAyNDI3NzkxNA=="]

fetchDetectObjects :: String -> IO (Response ImagaTagResponse)
fetchDetectObjects uri = do
  let request =
        setRequestQueryString [("image_url", Just convertedUri)] $
          setContentTypeJSON $
            setAuthorizationHeader
              "GET https://api.imagga.com/v2/tags"
  httpJSON request :: IO (Response ImagaTagResponse)
  where
    convertedUri = packStr'' uri

parseDetectedObjectsFromResponse :: ImagaTagResponse -> [Text]
parseDetectedObjectsFromResponse response =
  map tagStr itags
  where
    iresults = result response
    itags = tags iresults
    tagStr itag = en $ tag itag

packStr'' :: String -> ByteString
packStr'' = encodeUtf8 . pack
