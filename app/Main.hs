module Main where

import Handler
import Router
import qualified Server (run)

main :: IO ()
main = do
  let host = "localhost"
  let port = "3000"
  let router = Router.routeRequest
  let handler = Handler.createHandler router

  Server.run host port handler
