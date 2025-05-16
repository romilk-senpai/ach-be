{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppEnv (AppEnv (..))
import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Handler
import Internal.Board.Handlers (getAllBoards)
import Internal.Post.Handlers (getTopicPosts)
import Internal.Topic.Handlers (getBoardTopics)
import Middleware.Cors (corsMiddleware)
import Router
import Server (run)

main :: IO ()
main = do
  let host = "127.0.0.1"
  let port = "3000"

  conn <- connectPostgreSQL "host=localhost port=5432 user=postgres password=postgres dbname=ach_db"
  let env = AppEnv conn

  let router =
        addRoute ("GET", ["boards"]) [] (getAllBoards env) $
          addRoute ("GET", ["topics"]) [] (getBoardTopics env) $
            addRoute ("GET", ["posts"]) [] (getTopicPosts env) $
              addMiddleware corsMiddleware $
                Router Map.empty []

  let handler = Handler.createHandler router

  Server.run host port handler
