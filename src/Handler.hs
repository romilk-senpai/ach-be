{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( Handler,
    createHandler,
  )
where

import Common (httpErr)
import Control.Exception (SomeException ())
import Control.Exception.Base (try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.Text (intercalate)
import qualified Data.Text as Data
import Network.HTTP.Types (Header, Method, RequestHeaders, badRequest400, decodePath, hContentLength, internalServerError500)
import Network.HTTP.Types.Method (methodPost)
import Network.Socket (Socket, close)
import Network.Socket.ByteString (recv, sendAll)
import Request (DecodedPath, createRequest)
import Response (Response (resStatusCode), encodeResponse)
import Router (Router, matchRoute)

type Handler = Socket -> IO ()

createHandler :: Router -> Socket -> IO ()
createHandler router sock = do
  eHeaders <- extractHeaders sock
  case eHeaders of
    Left headerErr -> do
      putStrLn $ "[ERROR]: Failed to read headers: " ++ headerErr
      sendAll sock (encodeResponse (httpErr badRequest400 BS.empty))
      close sock
    Right (method, path, headers) -> do
      body <-
        if method == methodPost
          then case extractContentLength headers of
            Nothing -> do
              putStrLn "[ERROR]: Failed to parse content length"
              sendAll sock (encodeResponse (httpErr badRequest400 BS.empty))
              close sock
              fail "Failed to parse content length"
            Just contentLength -> readBody sock contentLength
          else return BS.empty

      let handler = matchRoute router (method, combineWithSlash (fst path))
          req = createRequest method path headers body
      responseResult <- try (handler req) :: IO (Either SomeException Response)
      case responseResult of
        Left ex -> do
          putStrLn $ "[ERROR]: Handler exception: " ++ show ex
          sendAll sock (encodeResponse (httpErr internalServerError500 "Internal Server Error"))
          close sock
        Right response -> do
          let bsResponse = encodeResponse response
          putStrLn $ "[INFO]: " ++ show (resStatusCode response)
          sendAll sock bsResponse
          close sock

combineWithSlash :: [Data.Text] -> Data.Text
combineWithSlash = intercalate "/"

extractContentLength :: RequestHeaders -> Maybe Int
extractContentLength headers = do
  len <- lookup hContentLength headers
  case C8.readInt len of
    Just (n, _) -> Just n
    Nothing -> Nothing

extractHeaders :: Socket -> IO (Either String (Method, DecodedPath, RequestHeaders))
extractHeaders sock = do
  bs <- readHeaders sock
  let (bsHeaders, _) = splitHeadersAndBody bs
  case parseHeaders bsHeaders of
    Left parseErr -> return (Left parseErr)
    Right headers -> return (Right headers)

readHeaders :: Socket -> IO BS.ByteString
readHeaders sock = go BS.empty
  where
    go acc = do
      chunk <- recv sock 1
      if BS.null chunk
        then return acc
        else
          let acc' = acc <> chunk
           in if "\r\n\r\n" `BS.isInfixOf` acc'
                then return acc'
                else go acc'

parseHeaders :: BS.ByteString -> Either String (Method, DecodedPath, RequestHeaders)
parseHeaders bs =
  case parseRequestLine bs of
    Left err -> Left err
    Right (method, path) -> do
      let headerLines = drop 1 (C8.lines bs)
          parsed = mapM parseHeaderLine headerLines
       in case parsed of
            Just headers -> Right (method, path, headers)
            Nothing -> Left "Broken headers"

parseRequestLine :: BS.ByteString -> Either String (Method, DecodedPath)
parseRequestLine request =
  case C8.lines request of
    (reqLine : _) ->
      case C8.words reqLine of
        (method : path : _) -> Right (method, decodePath path)
        _ -> Left "Broken request line"
    _ -> Left "Empty reqest"

parseHeaderLine :: BS.ByteString -> Maybe Header
parseHeaderLine line =
  case C8.break (== ':') line of
    (name, rest) ->
      if BS.null rest
        then Nothing
        else Just (CI.mk name, C8.dropWhile (== ' ') (C8.drop 1 rest))

readBody :: Socket -> Int -> IO BS.ByteString
readBody _ 0 = return BS.empty
readBody sock total = go total []
  where
    go 0 chunks = return $ BS.concat (reverse chunks)
    go remaining chunks = do
      chunk <- recv sock remaining
      if BS.null chunk
        then return $ BS.concat (reverse chunks)
        else go (remaining - BS.length chunk) (chunk : chunks)

splitHeadersAndBody :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitHeadersAndBody bs =
  let (headers, rest) = BS.breakSubstring "\r\n\r\n" bs
      body = BS.drop 4 rest
   in (headers, body)
