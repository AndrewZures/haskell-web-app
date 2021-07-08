{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Imagga where

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

newtype ImaggaTagDetails = ImaggaTagDetails
  { en :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ImaggaTagData = ImaggaTagData
  { confidence :: Float,
    tag :: ImaggaTagDetails
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ImaggaInternalResults = ImaggaResult
  { tags :: [ImaggaTagData]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ImaggaTagsResponse = ImaggaTagsResponse
  { result :: ImaggaInternalResults
  }
  deriving (Show, Generic, ToJSON, FromJSON)

setContentTypeJSON = setRequestHeader "Content-Type" ["application/json"]

setAuthorizationHeader =
  setRequestHeader
    "Authorization"
    ["Basic YWNjX2UyNjFjNDRkZjM5YTkxZDpjNTg3Njg1OWExNmY3NTVlZmU3ZTlmNjAyNDI3NzkxNA=="]

fetchDetectObjects :: String -> IO ImaggaTagsResponse
fetchDetectObjects uri = do
  let request =
        setRequestQueryString [("image_url", Just convertedUri)] $
          setContentTypeJSON $
            setAuthorizationHeader
              "GET https://api.imagga.com/v2/tags"
  response <- httpJSON request :: IO (Response ImaggaTagsResponse)
  return $ getResponseBody response
  where
    convertedUri = stringToBytestring uri
