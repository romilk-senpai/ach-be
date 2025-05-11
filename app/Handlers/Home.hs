module Handlers.Home (homeHandler) where

import Common (http200)
import qualified Data.Text as T
import Router

homeHandler :: HandlerFn
homeHandler _ = return $ http200 (T.pack "Home")
