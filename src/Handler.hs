module Handler where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Router (Router)

type Handler = Socket -> IO ()

createHandler :: Router -> Handler
createHandler router = \sock -> do
  msg <- recv sock 4096
  let response = router msg
  sendAll sock response
  close sock
