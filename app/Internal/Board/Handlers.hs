module Internal.Board.Handlers where

import Common (httpJSON)
import qualified Data.Text as T
import Internal.Board (BoardDTO (..))
import Router (HandlerFn)

getAllBoards :: HandlerFn
getAllBoards _ = do
  let boards = [BoardDTO 0 (T.pack "Anime") 0, BoardDTO 1 (T.pack "Hentai") 0]
  return $ httpJSON boards
