{-# LANGUAGE OverloadedStrings #-}

module Router
  ( HandlerFn,
    Middleware,
    RouteKey,
    RouteMap,
    Router (..),
    addMiddleware,
    addRoute,
    matchRoute,
  )
where

import Common (httpErr)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (Method, notFound404)
import Request (Request)
import Response (Response)

type HandlerFn = Request -> IO Response

type Middleware = HandlerFn -> HandlerFn

type RouteKey = (Method, [T.Text])

type RouteMap = Map.Map RouteKey HandlerFn

data Router = Router
  { routes :: RouteMap,
    middlewares :: [Middleware]
  }

addMiddleware :: Middleware -> Router -> Router
addMiddleware mw router = router {middlewares = mw : middlewares router}

addRoute :: RouteKey -> [Middleware] -> HandlerFn -> Router -> Router
addRoute routeKey routeMiddlewares handler router =
  let finalHandler = applyMiddlewares handler (middlewares router ++ routeMiddlewares)
   in router {routes = Map.insert routeKey finalHandler (routes router)}

applyMiddlewares :: HandlerFn -> [Middleware] -> HandlerFn
applyMiddlewares = foldr (\mw acc -> mw acc)

matchRoute :: Router -> RouteKey -> HandlerFn
matchRoute (Router rs _) routeKey =
  fromMaybe errorHandler (Map.lookup routeKey rs)

errorHandler :: HandlerFn
errorHandler _ = pure (httpErr notFound404 "Poshel nahui")
