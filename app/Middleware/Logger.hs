{-# LANGUAGE OverloadedStrings #-}

module Middleware.Logger (loggerMiddleware) where

import qualified Data.ByteString.Char8 as C8
import Data.List (intercalate)
import qualified Data.Text as T
import Request (DecodedPath, Request (..))
import Router

loggerMiddleware :: Middleware
loggerMiddleware next req = do
  res <- next req
  printDecodedPath (reqPath req)
  return res

printDecodedPath :: DecodedPath -> IO ()
printDecodedPath (segments, query) =
  putStrLn $
    "[INFO]: REQUEST path: "
      ++ intercalate "/" (map T.unpack segments)
      ++ ", params: "
      ++ intercalate "; " [C8.unpack k ++ "=" ++ maybe "" C8.unpack v | (k, v) <- query]
