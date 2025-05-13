{-# LANGUAGE OverloadedStrings #-}

module Common
  ( defaultHeaders,
    http200,
    httpErr,
    httpJSON,
  )
where

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Response (Response (..))

defaultHeaders :: ByteString -> [Header]
defaultHeaders body = [(hContentType, "text/plain"), (hContentLength, C8.pack (show (BS.length body))), (hConnection, "close")]

http200 :: BS.ByteString -> Response
http200 body =
  Response
    { resStatusCode = ok200,
      resHeaders = defaultHeaders body,
      resBody = body
    }

httpErr :: Status -> BS.ByteString -> Response
httpErr status body =
  Response
    { resStatusCode = status,
      resHeaders = defaultHeaders body,
      resBody = body
    }

httpJSON :: (ToJSON a) => a -> Response
httpJSON val =
  Response
    { resStatusCode = ok200,
      resHeaders = [("Content-Type", "application/json")],
      resBody = BL.toStrict (encode val)
    }
