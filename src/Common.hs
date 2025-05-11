{-# LANGUAGE OverloadedStrings #-}

module Common
  ( defaultHeaders,
    http200,
    httpErr,
  )
where

import qualified Data.ByteString.Char8 as C8
import Data.Text (Text, length)
import Network.HTTP.Types
import Response (Response, createResponse)

defaultHeaders :: Text -> [Header]
defaultHeaders body = [(hContentType, "text/plain"), (hContentLength, C8.pack (show (Data.Text.length body))), (hConnection, "close")]

http200 :: Text -> Response
http200 body = createResponse ok200 (defaultHeaders body) body

httpErr :: Status -> Text -> Response
httpErr status body = createResponse status (defaultHeaders body) body
