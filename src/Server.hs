module Server (run) where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Handler
import Network.Socket hiding (openSocket)

run :: String -> String -> Handler -> IO ()
run host port handler = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      (Just host)
      (Just port)
  let serveraddr = head addrinfos

  bracket (openSocket serveraddr) close $ \sock -> do
    putStrLn $ "Listening on " ++ host ++ ":" ++ port ++ "..."
    forever $ do
      (conn, _) <- accept sock
      void $ forkFinally (handler conn) (\_ -> close conn)

openSocket :: AddrInfo -> IO Socket
openSocket serveraddr = do
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serveraddr)
  listen sock 128
  return sock
