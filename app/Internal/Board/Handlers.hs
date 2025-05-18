{-# LANGUAGE OverloadedStrings #-}

module Internal.Board.Handlers (getBoard) where

import AppEnv (AppEnv (..))
import Common (httpErr, httpJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Internal.Board.Storage as Storage
import Network.HTTP.Types (badRequest400)
import Request (Request (..))
import Router (HandlerFn)

extractSlug :: [(ByteString, Maybe ByteString)] -> Maybe String
extractSlug params =
  lookup "slug" params >>= fmap C8.unpack

getBoard :: AppEnv -> HandlerFn
getBoard env req = do
  let queryParams = snd (reqPath req)
  case extractSlug queryParams of
    Just slug -> do
      dto <- Storage.getBoard env slug
      return $ httpJSON dto
    Nothing ->
      return $ httpErr badRequest400 "Invalid slug (poshel nahui)"
