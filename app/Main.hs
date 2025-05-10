{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (http200)
import qualified Data.Map.Strict as Map
import Handler
import Router
import qualified Server (run)

homeHandler :: HandlerFn
homeHandler _ = do
  http200 "text/plain" "Home"

submitHandler :: HandlerFn
submitHandler _ = do
  http200 "text/plain" "submit"

aboutHandler :: HandlerFn
aboutHandler _ = do
  http200 "text/plain" "about"

main :: IO ()
main = do
  let host = "127.0.0.1"
  let port = "3001"

  let router =
        addRoute "GET" "/" homeHandler $
          addRoute "GET" "/about" aboutHandler $
            addRoute "POST" "/submit" submitHandler $
              Router Map.empty

  let handler = Handler.createHandler router

  Server.run host port handler
