{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Imaga where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( Response,
    getResponseBody,
    httpJSON,
    setRequestHeader,
    setRequestQueryString,
  )
import Util (stringToBytestring)

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

fetchDetectObjects :: String -> IO ImagaTagResponse
fetchDetectObjects uri = do
  let request =
        setRequestQueryString [("image_url", Just convertedUri)] $
          setContentTypeJSON $
            setAuthorizationHeader
              "GET https://api.imagga.com/v2/tags"
  response <- httpJSON request :: IO (Response ImagaTagResponse)
  return $ getResponseBody response
  where
    convertedUri = stringToBytestring uri
