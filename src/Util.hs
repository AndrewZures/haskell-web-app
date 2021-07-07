module Util where

import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import System.Random (randomIO)

newUUID :: IO UUID
newUUID = randomIO

stringToBytestring :: String -> ByteString
stringToBytestring = encodeUtf8 . pack
