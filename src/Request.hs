module Request
  ( DecodedPath,
    Request (..),
    createRequest,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Types

type DecodedPath = ([Text], Query)

data Request = Request
  { reqMethod :: Method,
    reqPath :: DecodedPath,
    reqHeaders :: RequestHeaders,
    reqBody :: ByteString
  }

createRequest :: Method -> DecodedPath -> RequestHeaders -> ByteString -> Request
createRequest = Request
