{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Data
import Network.HTTP.Types (Header, Method, Query, RequestHeaders, ResponseHeaders, Status, hConnection, hContentLength, hContentType, ok200)

-- createRequest :: Method -> Path -> RequestHeaders -> Data.Text -> Request
-- createRequest method path headers body =
--   Request
--     { reqMethod = method,
--       reqPath = path,
--       reqHeaders = headers,
--       reqBody = body
--     }

type DecodedPath = ([Data.Text], Query)

data Request = Request
  { reqMethod :: Method,
    reqPath :: DecodedPath,
    reqHeaders :: RequestHeaders,
    reqBody :: Data.Text
  }

data Response = Response
  { resStatusCode :: Status,
    resHeaders :: ResponseHeaders,
    resBody :: Data.Text
  }

defaultHeaders :: Data.Text -> [Header]
defaultHeaders body = [(hContentType, "text/plain"), (hContentLength, C8.pack (show (Data.length body))), (hConnection, "close")]

http200 :: Data.Text -> Response
http200 body =
  Response
    { resStatusCode = ok200,
      resHeaders = defaultHeaders body,
      resBody = body
    }

httpErr :: Status -> Data.Text -> Response
httpErr status body =
  Response
    { resStatusCode = status,
      resHeaders = defaultHeaders body,
      resBody = body
    }
