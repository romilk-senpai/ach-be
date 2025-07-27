{-# LANGUAGE OverloadedStrings #-}

module Internal.Thread.Handlers (getBoardThreads, createThread) where

import AppEnv (AppEnv (..))
import Common (httpErr, httpJSON)
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Internal.Post (PostBody (..))
import Internal.Thread (ThreadBody (bodyOpPost))
import Internal.Thread.Storage (getThreads)
import qualified Internal.Thread.Storage as Storage
import Network.HTTP.Types (badRequest400)
import Request (Request (..))
import Router (HandlerFn)
import Text.Read (readMaybe)

extractBoardId :: [(ByteString, Maybe ByteString)] -> Maybe Int
extractBoardId params =
  lookup "boardId" params >>= (>>= readMaybe . C8.unpack)

getBoardThreads :: AppEnv -> HandlerFn
getBoardThreads env req = do
  let queryParams = snd (reqPath req)
  case extractBoardId queryParams of
    Just boardId -> do
      dtos <- getThreads env boardId
      return $ httpJSON dtos
    Nothing ->
      return $ httpErr badRequest400 "Invalid boardId (poshel nahui)"

createThread :: AppEnv -> HandlerFn
createThread env req = do
  let queryParams = snd (reqPath req)
      b = reqBody req
  let result = eitherDecode (BL.fromStrict b) :: Either String ThreadBody
  case result of
    Right pBody -> do
      case extractBoardId queryParams of
        Just boardId -> do
          let opPost = bodyOpPost pBody
              subject = bodySubject opPost
              author = bodyAuthor opPost
              content = bodyContent opPost
              media = bodyMedia opPost
          if null media
            then return $ httpErr badRequest400 "One media is required for op Post"
            else do
              dto <- Storage.createThread env boardId subject author content media
              return $ httpJSON dto
        Nothing ->
          return $ httpErr badRequest400 "Invalid threadId (poshel nahui)"
    Left _ -> return $ httpErr badRequest400 "poshel nahui 2"
