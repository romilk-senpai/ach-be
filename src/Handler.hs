{-# LANGUAGE OverloadedStrings #-}

module Handler where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Router (Router, matchRoute)

type Handler = Socket -> IO ()

createHandler :: Router -> Socket -> IO ()
createHandler router sock = do
  msg <- recv sock 4096
  let (method, path) = case parseRequestLine msg of
        Just result -> result
        Nothing -> ("GET", "error")
  let handler = matchRoute router method path
  sendAll sock (handler msg)
  close sock

parseRequestLine :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseRequestLine bs =
  case C8.lines bs of
    (reqLine : _) ->
      case C8.words reqLine of
        (method : path : _) -> Just (method, path)
        _ -> Nothing
    _ -> Nothing
