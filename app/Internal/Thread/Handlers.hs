{-# LANGUAGE OverloadedStrings #-}

module Internal.Thread.Handlers where

import AppEnv (AppEnv (..))
import Common (httpErr, httpJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Thread (Thread, createThreadDTO)
import Network.HTTP.Types (badRequest400)
import Request (Request (..))
import Router (HandlerFn)
import Text.Read (readMaybe)

extractBoardId :: [(ByteString, Maybe ByteString)] -> Maybe Int
extractBoardId params =
  lookup "boardId" params >>= (>>= readMaybe . C8.unpack)

getBoardThreads :: AppEnv -> HandlerFn
getBoardThreads env req = do
  let conn = dbConn env
      queryParams = snd (reqPath req)
  case extractBoardId queryParams of
    Just boardId -> do
      threads <- query conn "SELECT * FROM threads WHERE board_id = ?" (Only boardId)
      dtos <- mapM (createThreadDTO env) (threads :: [Thread])
      return $ httpJSON dtos
    Nothing ->
      return $ httpErr badRequest400 "Invalid boardId (poshel nahui)"
