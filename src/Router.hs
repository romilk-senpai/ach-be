{-# LANGUAGE OverloadedStrings #-}

module Router where

import Common (Request, Response, http200)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.HTTP.Types (Method)

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
  case Map.lookup routeKey rs of
    Just handleFn -> handleFn
    Nothing -> errorHandler

errorHandler :: HandlerFn
errorHandler _ = pure (http200 (T.pack "Poshel nahui"))
