{-# LANGUAGE OverloadedStrings #-}

module Internal.Post.Handlers where

import AppEnv (AppEnv (..))
import Common (httpErr, httpJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Post (Post, postToDTO)
import Network.HTTP.Types (badRequest400)
import Request (Request (..))
import Router (HandlerFn)
import Text.Read (readMaybe)

extractTopicId :: [(ByteString, Maybe ByteString)] -> Maybe Int
extractTopicId params =
  lookup "topicId" params >>= (>>= readMaybe . C8.unpack)

getTopicPosts :: AppEnv -> HandlerFn
getTopicPosts env req = do
  let conn = dbConn env
      queryParams = snd (reqPath req)
  case extractTopicId queryParams of
    Just topicId -> do
      topics <- query conn "SELECT * FROM posts WHERE topic_id = ?" (Only topicId)
      let dtos = map postToDTO (topics :: [Post])
      return $ httpJSON dtos
    Nothing ->
      return $ httpErr badRequest400 "Invalid boardId (poshel nahui)"
