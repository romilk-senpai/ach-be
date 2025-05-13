{-# LANGUAGE OverloadedStrings #-}

module Response
  ( Response (..),
    encodeResponse,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types

data Response = Response
  { resStatusCode :: Status,
    resHeaders :: ResponseHeaders,
    resBody :: BS.ByteString
  }

encodeResponse :: Response -> BS.ByteString
encodeResponse (Response status headers body) =
  let statusLine = encodeStatusLine status
      headerLines = encodeHeaders headers
      bodyBytes = body
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
