{-# LANGUAGE OverloadedStrings #-}

module Internal.BoardInfo.Storage (getBoardInfo) where

import AppEnv (AppEnv (..))
import Database.PostgreSQL.Simple (query_)
import Internal.BoardInfo (BoardInfo, BoardInfoDTO, boardInfoToDTO)

getBoardInfo :: AppEnv -> IO [BoardInfoDTO]
getBoardInfo env = do
  let conn = dbConn env
  boards <- query_ conn "SELECT b.id, b.name, b.category_id, c.name AS category_name, b.slug FROM boards b JOIN categories c ON b.category_id = c.id"
  return $ map boardInfoToDTO (boards :: [BoardInfo])
