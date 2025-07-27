{-# LANGUAGE OverloadedStrings #-}

module Internal.Board.Storage (getBoard) where

import AppEnv (AppEnv (..))
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Board (BoardDTO, createBoardDTO)

getBoard :: AppEnv -> String -> IO BoardDTO
getBoard env slug = do
  let conn = dbConn env
  boards <- query conn "SELECT b.id, b.name, b.category_id, c.name AS category_name, b.slug, b.description FROM boards b JOIN categories c ON b.category_id = c.id WHERE b.slug = ?" (Only slug)
  putStrLn "before trying to create board dto"
  case boards of
    [board] -> createBoardDTO env board
    _ -> fail "Board not found or multiple boards returned"
