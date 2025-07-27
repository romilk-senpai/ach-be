{-# LANGUAGE OverloadedStrings #-}

module Internal.Media.Storage (addMedia, getMedia) where

import AppEnv (AppEnv (..))
import Database.PostgreSQL.Simple
import Internal.Media (MediaFile, MediaFileDTO, mediaFileToDTO)

addMedia :: AppEnv -> String -> String -> String -> IO MediaFileDTO
addMedia env fileName filePath contentType = do
  let conn = dbConn env
  [media] <- query conn "INSERT INTO media (file_name, file_path, content_type) VALUES (?, ?, ?) RETURNING id, created_at, file_name, file_path, content_type" (fileName, filePath, contentType)
  return $ mediaFileToDTO media

getMedia :: AppEnv -> Int -> IO MediaFile
getMedia env mediaId = do
  let conn = dbConn env
  results <-
    query
      conn
      "SELECT id, created_at, file_name, file_path, content_type FROM media WHERE id = ?"
      (Only mediaId)
  case results of
    [media] -> return media
    _ -> fail "Media not found"
