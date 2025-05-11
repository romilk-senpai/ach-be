{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (http200)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Handler
import Handlers.Home (homeHandler)
import Middleware.Cors (corsMiddleware)
import Router
import qualified Server (run)

submitHandler :: HandlerFn
submitHandler _ = return $ http200 (T.pack "submit")

aboutHandler :: HandlerFn
aboutHandler _ = return $ http200 (T.pack "about")

main :: IO ()
main = do
  let host = "127.0.0.1"
  let port = "3001"

  let router =
        addRoute ("GET", []) [] homeHandler $
          addRoute ("GET", ["about", "jopa"]) [] aboutHandler $
            addRoute ("POST", ["submit"]) [] submitHandler $
              addMiddleware corsMiddleware $
                Router Map.empty []

  let handler = Handler.createHandler router

  Server.run host port handler
