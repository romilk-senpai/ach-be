{-# LANGUAGE OverloadedStrings #-}

module Internal.Post.Handlers
  ( getThreadPosts,
    getThreadPreview,
    createPost,
  )
where

import AppEnv (AppEnv (..))
import Common (httpErr, httpJSON)
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Post (Post (..), PostBody (..), PostDTO, postToDTO)
import Network.HTTP.Types (badRequest400)
import Request (Request (..))
import Router (HandlerFn)
import Text.Read (readMaybe)

extractThreadId :: [(ByteString, Maybe ByteString)] -> Maybe Int
extractThreadId params =
  lookup "threadId" params >>= (>>= readMaybe . C8.unpack)

getThreadPosts :: AppEnv -> HandlerFn
getThreadPosts env req = do
  let conn = dbConn env
  let queryParams = snd (reqPath req)
  case extractThreadId queryParams of
    Just threadId -> do
      posts <- query conn "SELECT * FROM posts WHERE thread_id = ?" (Only threadId)
      let dtos = map postToDTO (posts :: [Post])
      return $ httpJSON dtos
    Nothing ->
      return $ httpErr badRequest400 "Invalid threadId (poshel nahui)"

getThreadPreview :: AppEnv -> Int -> IO [PostDTO]
getThreadPreview env threadId = do
  let conn = dbConn env
  threads <- query conn "SELECT * FROM posts WHERE thread_id = ? ORDER BY id DESC LIMIT 5" (Only threadId)
  return (map postToDTO (threads :: [Post]))

createPost :: AppEnv -> HandlerFn
createPost env req = do
  let conn = dbConn env
  let queryParams = snd (reqPath req)
      b = reqBody req
  let result = eitherDecode (BL.fromStrict (TE.encodeUtf8 b)) :: Either String PostBody
  case result of
    Right pBody -> do
      case extractThreadId queryParams of
        Just threadId -> do
          let author = bodyAuthor pBody
              content = bodyContent pBody
          [post] <- query conn "INSERT INTO posts (thread_id, author, content) VALUES (?, ?, ?) RETURNING id, thread_id, created_at, topic, author, content" (threadId, author, content)
          return $ httpJSON (postToDTO post)
        Nothing ->
          return $ httpErr badRequest400 "Invalid threadId (poshel nahui)"
    Left _ -> return $ httpErr badRequest400 "Invalid request body (poshel nahui)"
