{-# LANGUAGE OverloadedStrings #-}

module Internal.Post.Handlers
  ( getThreadPosts,
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
import Internal.Post (PostBody (..))
import qualified Internal.Post.Storage as Storage
import Network.HTTP.Types (badRequest400)
import Request (Request (..))
import Router (HandlerFn)
import Text.Read (readMaybe)

extractThreadId :: [(ByteString, Maybe ByteString)] -> Maybe Int
extractThreadId params =
  lookup "threadId" params >>= (>>= readMaybe . C8.unpack)

getThreadPosts :: AppEnv -> HandlerFn
getThreadPosts env req = do
  let queryParams = snd (reqPath req)
  case extractThreadId queryParams of
    Just threadId -> do
      dtos <- Storage.getThreadPosts env threadId
      return $ httpJSON dtos
    Nothing ->
      return $ httpErr badRequest400 "Invalid threadId (poshel nahui)"

createPost :: AppEnv -> HandlerFn
createPost env req = do
  let queryParams = snd (reqPath req)
      b = reqBody req
  let result = eitherDecode (BL.fromStrict (TE.encodeUtf8 b)) :: Either String PostBody
  case result of
    Right pBody -> do
      case extractThreadId queryParams of
        Just threadId -> do
          let author = bodyAuthor pBody
              content = bodyContent pBody
              subject = bodySubject pBody
          post <- Storage.createPost env threadId subject author content
          return $ httpJSON post
        Nothing ->
          return $ httpErr badRequest400 "Invalid threadId (poshel nahui)"
    Left _ -> return $ httpErr badRequest400 "Invalid request body (poshel nahui)"
