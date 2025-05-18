{-# LANGUAGE OverloadedStrings #-}

module Internal.Thread.Storage (getThreads) where

import AppEnv (AppEnv (..))
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Thread (ThreadDTO, createThreadDTO)

getThreads :: AppEnv -> Int -> IO [ThreadDTO]
getThreads env boardId = do
  let conn = dbConn env
  threads <- query conn "SELECT * FROM threads WHERE board_id = ?" (Only boardId)
  mapM (createThreadDTO env) threads
