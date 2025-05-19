{-# LANGUAGE OverloadedStrings #-}

module Internal.Media.Storage (addMedia) where

import AppEnv (AppEnv (..))
import Database.PostgreSQL.Simple
import Internal.Media (MediaFileDTO, mediaFileToDTO)

addMedia :: AppEnv -> String -> String -> IO MediaFileDTO
addMedia env fileName filePath = do
  let conn = dbConn env
  [media] <- query conn "INSERT INTO media (file_name, file_path) VALUES (?, ?) RETURNING id, created_at, file_path, file_name" (fileName, filePath)
  return $ mediaFileToDTO media
