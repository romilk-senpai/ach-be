{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

type Path = BS.ByteString

type Method = BS.ByteString

http200 :: BS.ByteString -> BS.ByteString -> BS.ByteString
http200 contentType body =
  BS.concat
    [ "HTTP/1.1 200 OK\r\n",
      "Content-Type: ",
      contentType,
      "\r\n",
      "Content-Length: ",
      C8.pack (show (BS.length body)),
      "\r\n",
      "Connection: close\r\n",
      "\r\n",
      body
    ]
