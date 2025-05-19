{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppEnv (AppEnv (..))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import Data.String (fromString)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Handler
import Internal.Board.Handlers (getBoard)
import Internal.BoardInfo.Handlers (getBoardInfo)
import Internal.Media.Handlers (uploadMedia)
import Internal.Post.Handlers (createPost, getThreadPosts)
import Internal.Thread.Handlers (createThread, getBoardThreads)
import Middleware.Cors (corsMiddleware)
import Middleware.Logger (loggerMiddleware)
import Router
import Server (run)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  let host = "0.0.0.0"
  let port = "8080"

  maybeUrl <- lookupEnv "DATABASE_URL"
  let connStr = Data.Maybe.fromMaybe "host=localhost port=5432 user=postgres password=postgres dbname=ach_db" maybeUrl
  conn <- connectPostgreSQL (fromString connStr)

  putStrLn connStr

  let env = AppEnv conn

  let router =
        addRoute ("GET", "boards") (getBoardInfo env) $
          addRoute ("GET", "board") (getBoard env) $
            addRoute ("GET", "threads") (getBoardThreads env) $
              addRoute ("POST", "createThread") (createThread env) $
                addRoute ("GET", "posts") (getThreadPosts env) $
                  addRoute ("POST", "createPost") (createPost env) $
                    addRoute ("POST", "uploadMedia") (uploadMedia env) $
                      addMiddleware corsMiddleware $
                        addMiddleware loggerMiddleware $
                          Router Map.empty []

  let handler = Handler.createHandler router

  Server.run host port handler
