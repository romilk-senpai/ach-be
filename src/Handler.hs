{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( Handler,
    createHandler,
  )
where

import Common (httpErr)
import Control.Exception (SomeException)
import Control.Exception.Base (try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.Text (intercalate)
import qualified Data.Text as Data
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Header, Method, RequestHeaders, badRequest400, decodePath, internalServerError500)
import Network.Socket (Socket, close)
import Network.Socket.ByteString (recv, sendAll)
import Request (DecodedPath, Request (..), createRequest)
import Response (Response, encodeResponse)
import Router (Router, matchRoute)

type Handler = Socket -> IO ()

createHandler :: Router -> Socket -> IO ()
createHandler router sock = do
  result <- try $ readRequest sock :: IO (Either SomeException BS.ByteString)

  case result of
    Left _ -> do
      putStrLn "Failed to read request"
      sendAll sock (encodeResponse (httpErr badRequest400 BS.empty))
      close sock
    Right bs -> do
      case parseRequest bs of
        Left err -> do
          putStrLn $ "Failed to parse request: " ++ err
          sendAll sock (encodeResponse (httpErr badRequest400 (C8.pack err)))
          close sock
        Right req -> do
          let method = reqMethod req
          let (segments, _) = reqPath req
          let handler = matchRoute router (method, combineWithSlash segments)

          responseResult <- try (handler req) :: IO (Either SomeException Response)
          case responseResult of
            Left ex -> do
              putStrLn $ "Handler exception: " ++ show ex
              sendAll sock (encodeResponse (httpErr internalServerError500 "Internal Server Error"))
              close sock
            Right response -> do
              let bsResponse = encodeResponse response
              sendAll sock bsResponse
              close sock

combineWithSlash :: [Data.Text] -> Data.Text
combineWithSlash = intercalate "/"

readRequest :: Socket -> IO BS.ByteString
readRequest sock = go BS.empty
  where
    go acc = do
      chunk <- recv sock 4096
      if BS.null chunk
        then return acc
        else
          let acc' = acc <> chunk
           in if "\r\n\r\n" `BS.isInfixOf` acc'
                then return acc'
                else go acc'

parseRequest :: BS.ByteString -> Either String Request
parseRequest bs = do
  let (bs_headers, bs_body) = splitHeadersAndBody bs
  (method, path) <- parseRequestLine bs_headers
  headers <- parseHeaders bs_headers
  let body = TE.decodeUtf8 bs_body
  return $ createRequest method path headers body

splitHeadersAndBody :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitHeadersAndBody bs =
  let (headers, rest) = BS.breakSubstring "\r\n\r\n" bs
      body = BS.drop 4 rest
   in (headers, body)

parseRequestLine :: BS.ByteString -> Either String (Method, DecodedPath)
parseRequestLine request =
  case C8.lines request of
    (reqLine : _) ->
      case C8.words reqLine of
        (method : path : _) -> Right (method, decodePath path)
        _ -> Left "Broken request line"
    _ -> Left "Empty reqest"

parseHeaders :: BS.ByteString -> Either String RequestHeaders
parseHeaders bs =
  let headerLines = drop 1 (C8.lines bs)
      parsed = mapM parseHeaderLine headerLines
   in case parsed of
        Just hs -> Right hs
        Nothing -> Left "Broken headers"

parseHeaderLine :: BS.ByteString -> Maybe Header
parseHeaderLine line =
  case C8.break (== ':') line of
    (name, rest) ->
      if BS.null rest
        then Nothing
        else Just (CI.mk name, C8.dropWhile (== ' ') (C8.drop 1 rest))
