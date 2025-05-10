module Main where

import qualified Server (someFunc)

main :: IO ()
main = do
  Server.someFunc
