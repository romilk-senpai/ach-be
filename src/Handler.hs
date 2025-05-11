{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Common (DecodedPath, createRequest, encodeResponse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Header, Method, RequestHeaders, decodePath)
import Network.Socket (Socket, close)
import Network.Socket.ByteString (recv, sendAll)
import Router (Router, matchRoute)

type Handler = Socket -> IO ()

createHandler :: Router -> Socket -> IO ()
createHandler router sock = do
  msg <- recv sock 4096
  let (bs_headers, bs_body) = splitHeadersAndBody msg
  let (method, path) = case parseRequestLine bs_headers of
        Just result -> result
        Nothing -> ("GET", ([pack "error"], []))

  let headers = parseHeaders bs_headers
  let body = TE.decodeUtf8 bs_body

  let request = createRequest method path headers body

  let (segments, _) = path
  let handler = matchRoute router (method, segments)

  response <- handler request
  let bsResponse = encodeResponse response

  C8.putStrLn bsResponse

  sendAll sock bsResponse
  close sock

splitHeadersAndBody :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitHeadersAndBody bs =
  let (headers, rest) = BS.breakSubstring "\r\n\r\n" bs
      body = BS.drop 4 rest
   in (headers, body)

parseRequestLine :: BS.ByteString -> Maybe (Method, DecodedPath)
parseRequestLine request =
  case C8.lines request of
    (reqLine : _) ->
      case C8.words reqLine of
        (method : path : _) -> Just (method, decodePath path)
        _ -> Nothing
    _ -> Nothing

parseHeaders :: BS.ByteString -> RequestHeaders
parseHeaders bs =
  let headerLines = drop 1 (C8.lines bs)
   in mapMaybe parseHeaderLine headerLines

parseHeaderLine :: BS.ByteString -> Maybe Header
parseHeaderLine line =
  case C8.break (== ':') line of
    (name, rest) ->
      if BS.null rest
        then Nothing
        else Just (CI.mk name, C8.dropWhile (== ' ') (C8.drop 1 rest))
