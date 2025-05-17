{-# LANGUAGE OverloadedStrings #-}

module Middleware.Logger (loggerMiddleware) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Request (DecodedPath, Request (..))
import Router

loggerMiddleware :: Middleware
loggerMiddleware next req = do
  res <- next req
  printDecodedPath (reqPath req)
  return res

printDecodedPath :: DecodedPath -> IO ()
printDecodedPath (segments, query) = do
  putStrLn "Path segments:"
  mapM_ (putStrLn . T.unpack) segments

  putStrLn "Query params:"
  mapM_ printQueryParam query
  where
    printQueryParam (keyBS, mValBS) = do
      let key = C8.unpack keyBS
          val = maybe "" C8.unpack mValBS
      putStrLn $ key ++ " = " ++ val
