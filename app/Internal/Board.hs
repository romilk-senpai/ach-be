{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Board
  ( Board (..),
    BoardDTO (..),
    createBoardDTO,
  )
where

import AppEnv (AppEnv)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)
import Internal.Thread (ThreadDTO)
import Internal.Thread.Storage (getThreads)

data Board = Board
  { boardId :: Int,
    boardName :: Text,
    boardCategoryId :: Int,
    boardCategoryName :: String,
    boardSlug :: Text,
    boardDescription :: Text
  }
  deriving (Show, Eq)

instance FromRow Board where
  fromRow = Board <$> field <*> field <*> field <*> field <*> field <*> field

data BoardDTO = BoardDTO
  { id :: Int,
    name :: Text,
    categoryName :: String,
    slug :: Text,
    description :: Text,
    threads :: [ThreadDTO]
  }
  deriving (Generic, ToJSON)

createBoardDTO :: AppEnv -> Board -> IO BoardDTO
createBoardDTO env board = do
  let bId = boardId board
      bName = boardName board
      bCatName = boardCategoryName board
      bSlug = boardSlug board
      bDesc = boardDescription board
  bThreads <- getThreads env bId
  return $ BoardDTO bId bName bCatName bSlug bDesc bThreads
