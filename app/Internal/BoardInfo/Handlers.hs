{-# LANGUAGE OverloadedStrings #-}

module Internal.BoardInfo.Handlers (getBoardInfo) where

import AppEnv (AppEnv (..))
import Common (httpJSON)
import Database.PostgreSQL.Simple (query_)
import Internal.BoardInfo (BoardInfo (..), boardInfoToDTO)
import Router (HandlerFn)

getBoardInfo :: AppEnv -> HandlerFn
getBoardInfo env _req = do
  boards <- query_ (dbConn env) "SELECT b.id, b.name, b.category_id, c.name AS category_name, b.slug FROM boards b JOIN categories c ON b.category_id = c.id"
  let dtos = map boardInfoToDTO (boards :: [BoardInfo])
  return $ httpJSON dtos
