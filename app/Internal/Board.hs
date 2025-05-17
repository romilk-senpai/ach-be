{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Board
  ( Board (..),
    BoardDTO (..),
    boardToDTO,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)

data Board = Board
  { boardId :: Int,
    boardName :: Text,
    boardCategoryId :: Int,
    boardSlug :: Text
  }
  deriving (Show, Eq)

instance FromRow Board where
  fromRow = Board <$> field <*> field <*> field <*> field

data BoardDTO = BoardDTO
  { id :: Int,
    name :: Text,
    categoryId :: Int,
    slug :: Text
  }
  deriving (Generic, ToJSON)

boardToDTO :: Board -> BoardDTO
boardToDTO (Board bId bName bCat bSlug) = BoardDTO bId bName bCat bSlug
