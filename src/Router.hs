{-# LANGUAGE OverloadedStrings #-}

module Router where

import Common (Request, Response, http200)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.HTTP.Types (Method)

type HandlerFn = Request -> Response

type RouteKey = (Method, [T.Text])

type RouteMap = Map.Map RouteKey HandlerFn

newtype Router = Router {routes :: RouteMap}

errorHandler :: HandlerFn
errorHandler _ = http200 (T.pack "Poshel nahui")

addRoute :: RouteKey -> HandlerFn -> Router -> Router
addRoute routeKey handler (Router rs) = Router (Map.insert routeKey handler rs)

matchRoute :: Router -> RouteKey -> HandlerFn
matchRoute (Router rs) routeKey = do
  case Map.lookup routeKey rs of
    Just handleFn -> handleFn
    Nothing -> errorHandler
