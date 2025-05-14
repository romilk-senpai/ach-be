module Server (run) where

import Control.Monad (forever)
import Handler
import Network.Socket

run :: String -> String -> Handler -> IO ()
run host port handler = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      (Just host)
      (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serveraddr)
  listen sock 1
  putStrLn $ "Listening on " ++ host ++ ":" ++ port ++ "..."

  forever $ do
    (conn, _) <- accept sock
    handler conn
