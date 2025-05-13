{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (http200)
import qualified Data.Map.Strict as Map
import Handler
import Handlers.Home (homeHandler)
import Internal.Board.Handlers (getAllBoards)
import Middleware.Cors (corsMiddleware)
import Router
import qualified Server (run)

submitHandler :: HandlerFn
submitHandler _ = return $ http200 "submit"

aboutHandler :: HandlerFn
aboutHandler _ = return $ http200 "about"

main :: IO ()
main = do
  let host = "127.0.0.1"
  let port = "3000"

  let router =
        addRoute ("GET", []) [] homeHandler $
          addRoute ("GET", ["getAllBoards"]) [] getAllBoards $
            addRoute ("GET", ["about", "jopa"]) [] aboutHandler $
              addRoute ("POST", ["submit"]) [] submitHandler $
                addMiddleware corsMiddleware $
                  Router Map.empty []

  let handler = Handler.createHandler router

  Server.run host port handler
