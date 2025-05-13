{-# LANGUAGE OverloadedStrings #-}

module Handlers.Home (homeHandler) where

import Common (http200)
import Router

homeHandler :: HandlerFn
homeHandler _ = return $ http200 "Home"
