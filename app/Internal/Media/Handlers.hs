{-# LANGUAGE OverloadedStrings #-}

module Internal.Media.Handlers (uploadMedia, downloadMedia) where

import AppEnv (AppEnv)
import Common (httpErr, httpJSON, httpMedia)
import Control.Exception (SomeException)
import Control.Exception.Base (try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Internal.Media (MediaFile (..))
import Internal.Media.Storage as Storage
import Network.HTTP.Types (RequestHeaders, badRequest400, hContentType)
import Network.HTTP.Types.Status (notFound404)
import Request (Request (..))
import Router (HandlerFn)
import System.Directory (doesFileExist)
import System.FilePath.Posix (takeExtension)
import Text.Read (readMaybe)

extractFileName :: [(ByteString, Maybe ByteString)] -> Maybe String
extractFileName params =
  lookup "fileName" params >>= fmap C8.unpack

extractContentType :: RequestHeaders -> Maybe ByteString
extractContentType headers = do
  lookup hContentType headers

isImageOrVideo :: ByteString -> Bool
isImageOrVideo ct =
  let ctStr = C8.unpack ct
   in "image/" `prefixOf` ctStr || "video/" `prefixOf` ctStr

prefixOf :: String -> String -> Bool
prefixOf prefix str = take (length prefix) str == prefix

uploadMedia :: AppEnv -> HandlerFn
uploadMedia env req = do
  case extractContentType (reqHeaders req) of
    Just eContentType -> do
      if isImageOrVideo eContentType
        then do
          let queryParams = snd (reqPath req)
          case extractFileName queryParams of
            Just fileName -> do
              let imgBytes = reqBody req
                  maxSize = 1 * 1024 * 1024
              if BS.length imgBytes > maxSize
                then return $ httpErr badRequest400 "File too large"
                else do
                  uuid <- UUIDv4.nextRandom
                  let ext = takeExtension fileName
                      uniqueName = UUID.toString uuid ++ ext
                      filePath = "uploads/" ++ uniqueName
                  BS.writeFile filePath imgBytes
                  dto <- Storage.addMedia env fileName filePath (C8.unpack eContentType)
                  return $ httpJSON dto
            Nothing -> return $ httpErr badRequest400 "Empty file name"
        else do return $ httpErr badRequest400 "Unsupported content-type"
    Nothing -> return $ httpErr badRequest400 "Invalid content-type header"

downloadMedia :: AppEnv -> HandlerFn
downloadMedia env req = do
  let queryParams = snd (reqPath req)
  case lookup "mediaId" queryParams >>= (>>= readMaybe . C8.unpack) of
    Just pMediaId -> do
      result <- try $ getMedia env pMediaId :: IO (Either SomeException MediaFile)
      case result of
        Right media -> do
          fileExists <- doesFileExist (mediaPath media)
          if fileExists
            then do
              content <- BS.readFile (mediaPath media)
              return $ httpMedia (C8.pack $ mediaName media) (C8.pack $ mediaContentType media) content
            else return $ httpErr notFound404 "File not found"
        Left _ -> return $ httpErr notFound404 "File not found"
    Nothing -> return $ httpErr badRequest400 "Missing or invalid mediaId"
