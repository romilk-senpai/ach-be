module Request
  ( DecodedPath,
    Request (..),
    createRequest,
  )
where

import Data.Text (Text)
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
