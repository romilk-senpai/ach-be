{-# LANGUAGE OverloadedStrings #-}

module Internal.Topic.Handlers where

import AppEnv (AppEnv (..))
import Common (httpErr, httpJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Topic (Topic, topicToDTO)
import Network.HTTP.Types (badRequest400)
import Request (Request (..))
import Router (HandlerFn)
import Text.Read (readMaybe)

extractBoardId :: [(ByteString, Maybe ByteString)] -> Maybe Int
extractBoardId params =
  lookup "boardId" params >>= (>>= readMaybe . C8.unpack)

getBoardTopics :: AppEnv -> HandlerFn
getBoardTopics env req = do
  let conn = dbConn env
      queryParams = snd (reqPath req)
  case extractBoardId queryParams of
    Just boardId -> do
      topics <- query conn "SELECT * FROM topics WHERE board_id = ?" (Only boardId)
      let dtos = map topicToDTO (topics :: [Topic])
      return $ httpJSON dtos
    Nothing ->
      return $ httpErr badRequest400 "Invalid boardId (poshel nahui)"
