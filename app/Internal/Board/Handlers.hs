{-# LANGUAGE OverloadedStrings #-}

module Internal.Board.Handlers where

import AppEnv (AppEnv (..))
import Common (httpJSON)
import Database.PostgreSQL.Simple (query_)
import Internal.Board (Board (..), boardToDTO)
import Router (HandlerFn)

getAllBoards :: AppEnv -> HandlerFn
getAllBoards env _req = do
  boards <- query_ (dbConn env) "SELECT * FROM boards"
  let dtos = map boardToDTO (boards :: [Board])
  return $ httpJSON dtos
