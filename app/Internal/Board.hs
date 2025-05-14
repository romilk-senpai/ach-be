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
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)

data Board = Board
  { boardId :: Int,
    boardName :: Text,
    boardCategory :: Int
  }
  deriving (Show, Eq)

instance FromRow Board where
  fromRow = Board <$> field <*> field <*> field

data BoardDTO = BoardDTO
  { id :: Int,
    name :: Text,
    category :: Int
  }
  deriving (Generic, ToJSON)

boardToDTO :: Board -> BoardDTO
boardToDTO (Board id name cat) = BoardDTO id name cat
