{-# LANGUAGE OverloadedStrings #-}

module Middleware.Cors (corsMiddleware) where

import Response
import Router

corsMiddleware :: Middleware
corsMiddleware next req = do
  res <- next req
  let updatedHeaders = ("Access-Control-Allow-Origin", "*") : resHeaders res
  return $ res {resHeaders = updatedHeaders}
