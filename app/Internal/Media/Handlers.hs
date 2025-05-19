{-# LANGUAGE OverloadedStrings #-}

module Internal.Media.Handlers where

import AppEnv (AppEnv)
import Common (httpErr, httpJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Internal.Media.Storage as Storage
import Network.HTTP.Types (RequestHeaders, badRequest400, hContentType)
import Request (Request (..))
import Router (HandlerFn)

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
    Just contentType -> do
      if isImageOrVideo contentType
        then do
          let queryParams = snd (reqPath req)
          case extractFileName queryParams of
            Just fileName -> do
              let imgBytes = BL.fromStrict (reqBody req)
                  maxSize = 1 * 1024 * 1024
              if BL.length imgBytes > maxSize
                then return $ httpErr badRequest400 "File too large"
                else do
                  let filePath = "uploads/" ++ fileName
                  BL.writeFile filePath imgBytes
                  dto <- Storage.addMedia env fileName filePath
                  return $ httpJSON dto
            Nothing -> return $ httpErr badRequest400 "Empty file name"
        else do return $ httpErr badRequest400 "Unsupported Content-Type"
    Nothing -> return $ httpErr badRequest400 "Invalid content-type header"
