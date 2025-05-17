{-# LANGUAGE OverloadedStrings #-}

module Middleware.Logger (loggerMiddleware) where

import qualified Data.ByteString.Char8 as C8
import Data.List (intercalate)
import qualified Data.Text as T
import Request (Request (..))
import Router

loggerMiddleware :: Middleware
loggerMiddleware next req = do
  res <- next req
  printRequestInfo req
  return res

printRequestInfo :: Request -> IO ()
printRequestInfo req = do
  let method = C8.unpack (reqMethod req)
  let (segments, query) = reqPath req
  putStrLn $
    "[INFO]: Request METHOD: "
      ++ method
      ++ ", PATH: "
      ++ intercalate "/" (map T.unpack segments)
      ++ ", PARAMS: "
      ++ intercalate "; " [C8.unpack k ++ "=" ++ maybe "" C8.unpack v | (k, v) <- query]
