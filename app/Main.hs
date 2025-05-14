{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Handler
import Handlers.Home (homeHandler)
import Internal.Board.Handlers (getAllBoards)
import Middleware.Cors (corsMiddleware)
import Router
import qualified Server (run)
import AppEnv (AppEnv(..))

main :: IO ()
main = do
  let host = "127.0.0.1"
  let port = "3000"

  conn <- connectPostgreSQL "host=localhost port=5432 user=postgres password=postgres dbname=ach_db"
  let appEnv = AppEnv conn

  let router =
        addRoute ("GET", []) [] homeHandler $
          addRoute ("GET", ["getAllBoards"]) [] (getAllBoards appEnv) $
            addMiddleware corsMiddleware $
              Router Map.empty []

  let handler = Handler.createHandler router

  Server.run host port handler
