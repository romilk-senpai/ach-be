module Internal.BoardInfo.Handlers (getBoardInfo) where

import AppEnv (AppEnv (..))
import Common (httpJSON)
import qualified Internal.BoardInfo.Storage as Storage
import Router (HandlerFn)

getBoardInfo :: AppEnv -> HandlerFn
getBoardInfo env _req = do
  dtos <- Storage.getBoardInfo env
  return $ httpJSON dtos
