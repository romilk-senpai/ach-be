{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.Text (Text, length)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types

type DecodedPath = ([Text], Query)

data Request = Request
  { reqMethod :: Method,
    reqPath :: DecodedPath,
    reqHeaders :: RequestHeaders,
    reqBody :: Text
  }

createRequest :: Method -> DecodedPath -> RequestHeaders -> Text -> Request
createRequest = Request

data Response = Response
  { resStatusCode :: Status,
    resHeaders :: ResponseHeaders,
    resBody :: Text
  }

createResponse :: Status -> ResponseHeaders -> Text -> Response
createResponse = Response

encodeResponse :: Response -> BS.ByteString
encodeResponse (Response status headers body) =
  let statusLine = encodeStatusLine status
      headerLines = encodeHeaders headers
      bodyBytes = TE.encodeUtf8 body
   in BS.concat
        [ statusLine,
          C8.pack "\r\n",
          BS.concat headerLines,
          C8.pack "\r\n",
          bodyBytes
        ]

encodeStatusLine :: Status -> BS.ByteString
encodeStatusLine status =
  C8.pack $ "HTTP/1.1 " ++ show (statusCode status) ++ " " ++ C8.unpack (statusMessage status)

encodeHeaders :: ResponseHeaders -> [BS.ByteString]
encodeHeaders = map encodeHeader

encodeHeader :: Header -> BS.ByteString
encodeHeader (name, value) =
  CI.original name <> ": " <> value <> "\r\n"

defaultHeaders :: Text -> [Header]
defaultHeaders body = [(hContentType, "text/plain"), (hContentLength, C8.pack (show (Data.Text.length body))), (hConnection, "close")]

http200 :: Text -> Response
http200 body =
  Response
    { resStatusCode = ok200,
      resHeaders = defaultHeaders body,
      resBody = body
    }

httpErr :: Status -> Text -> Response
httpErr status body =
  Response
    { resStatusCode = status,
      resHeaders = defaultHeaders body,
      resBody = body
    }
