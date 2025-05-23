{-# LANGUAGE OverloadedStrings #-}

module Internal.Post.Storage
  ( getThreadPosts,
    getThreadLastReplies,
    createPost,
    getOpPost,
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
  posts <- query conn "SELECT * FROM posts WHERE thread_id = ? ORDER BY id DESC LIMIT 6" (Only threadId) -- tee hee
  let p = reverse posts
      filtered =
        if length p <= 6
          then tail p
          else p
  return $ map postToDTO filtered

getOpPost :: AppEnv -> Int -> IO PostDTO
getOpPost env threadId = do
  let conn = dbConn env
  [post] <- query conn "SELECT * FROM posts WHERE thread_id = ? ORDER BY id ASC LIMIT 1" (Only threadId)
  return $ postToDTO post

createPost :: AppEnv -> Int -> Maybe Data.Text -> Maybe Data.Text -> Data.Text -> IO PostDTO
createPost env threadId subject author content = do
  let conn = dbConn env
  [post] <- query conn "INSERT INTO posts (thread_id, subject, author, content) VALUES (?, ?, ?, ?) RETURNING id, local_id, thread_id, board_id, created_at, subject, author, content" (threadId, subject, author, content)
  return $ postToDTO post
