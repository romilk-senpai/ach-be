{-# LANGUAGE OverloadedStrings #-}

module Internal.Post.Storage
  ( getThreadPosts,
    getThreadLastReplies,
    createPost,
  )
where

import AppEnv (AppEnv (..))
import qualified Data.Text as Data
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Post (PostDTO, postToDTO)

getThreadPosts :: AppEnv -> Int -> IO [PostDTO]
getThreadPosts env threadId = do
  let conn = dbConn env
  posts <- query conn "SELECT * FROM posts WHERE thread_id = ?" (Only threadId)
  return $ map postToDTO posts

getThreadLastReplies :: AppEnv -> Int -> IO [PostDTO]
getThreadLastReplies env threadId = do
  let conn = dbConn env
  posts <- query conn "SELECT * FROM posts WHERE thread_id = ? ORDER BY id ASC LIMIT 5" (Only threadId)
  return $ map postToDTO posts

createPost :: AppEnv -> Int -> Maybe Data.Text -> Data.Text -> IO PostDTO
createPost env threadId author content = do
  let conn = dbConn env
  [post] <- query conn "INSERT INTO posts (thread_id, author, content) VALUES (?, ?, ?) RETURNING id, thread_id, created_at, topic, author, content" (threadId, author, content)
  return $ postToDTO post
