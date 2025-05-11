{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (http200)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Handler
import Router
import qualified Server (run)

homeHandler :: HandlerFn
homeHandler _ = http200 (T.pack "Home")

submitHandler :: HandlerFn
submitHandler _ = http200 (T.pack "submit")

aboutHandler :: HandlerFn
aboutHandler _ = http200 (T.pack "about")

main :: IO ()
main = do
  let host = "127.0.0.1"
  let port = "3001"

  let router =
        addRoute ("GET", []) homeHandler $
          addRoute ("GET", ["about"]) aboutHandler $
            addRoute ("POST", ["submit"]) submitHandler $
              Router Map.empty

  let handler = Handler.createHandler router

  Server.run host port handler
