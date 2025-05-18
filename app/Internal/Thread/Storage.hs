{-# LANGUAGE OverloadedStrings #-}

module Internal.Thread.Storage (getThreads, createThread) where

import AppEnv (AppEnv (..))
import Data.Text (Text)
import Database.PostgreSQL.Simple (execute, query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Thread (Thread (..), ThreadDTO (), createThreadDTO)

getThreads :: AppEnv -> Int -> IO [ThreadDTO]
getThreads env boardId = do
  let conn = dbConn env
  threads <- query conn "SELECT * FROM threads WHERE board_id = ?" (Only boardId)
  mapM (createThreadDTO env) threads

createThread :: AppEnv -> Int -> Maybe Text -> Maybe Text -> Text -> IO ThreadDTO
createThread env boardId subject author content = do
  let conn = dbConn env
  [thread] <- query conn "INSERT INTO threads (board_id) VALUES (?) RETURNING id, board_id" (Only boardId) :: IO [Thread]
  let tId = threadId thread
  _ <- execute conn "INSERT INTO posts (thread_id, subject, author, content) VALUES (?, ?, ?, ?)" (tId, subject, author, content)
  createThreadDTO env thread
