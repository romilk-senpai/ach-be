{-# LANGUAGE OverloadedStrings #-}

module Internal.Thread.Handlers (getBoardThreads, createThread) where

import AppEnv (AppEnv (..))
import Common (httpErr, httpJSON)
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (execute, query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Internal.Post (PostBody (..))
import Internal.Thread (Thread (..), ThreadBody (bodyOpPost), createThreadDTO)
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

createThread :: AppEnv -> HandlerFn
createThread env req = do
  let conn = dbConn env
  let queryParams = snd (reqPath req)
      b = reqBody req
  let result = eitherDecode (BL.fromStrict (TE.encodeUtf8 b)) :: Either String ThreadBody
  case result of
    Right pBody -> do
      case extractBoardId queryParams of
        Just boardId -> do
          [thread] <- query conn "INSERT INTO threads (board_id) VALUES (?) RETURNING id, board_id" (Only boardId) :: IO [Thread]
          let opPost = bodyOpPost pBody
          let author = bodyAuthor opPost
          let content = bodyContent opPost
          let tId = threadId thread
          _ <- execute conn "INSERT INTO posts (thread_id, author, content) VALUES (?, ?, ?)" (tId, author, content)
          dto <- createThreadDTO env thread
          return $ httpJSON dto
        Nothing ->
          return $ httpErr badRequest400 "Invalid threadId (poshel nahui)"
    Left _ -> return $ httpErr badRequest400 "poshel nahui 2"
