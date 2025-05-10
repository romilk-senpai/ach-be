{-# LANGUAGE OverloadedStrings #-}

module Router where

import Common (Method, Path, http200)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

type HandlerFn = BS.ByteString -> BS.ByteString

type RouteKey = (Method, Path)

type RouteMap = Map.Map RouteKey HandlerFn

newtype Router = Router {routes :: RouteMap}

errorHandler :: HandlerFn
errorHandler _ = do
  http200 "text/plain" "Error"

addRoute :: Method -> Path -> HandlerFn -> Router -> Router
addRoute method path handler (Router rs) = Router (Map.insert (method, path) handler rs)

matchRoute :: Router -> Method -> Path -> HandlerFn
matchRoute (Router rs) method path = do
  case Map.lookup (method, path) rs of
    Just handleFn -> handleFn
    Nothing -> errorHandler
