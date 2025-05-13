{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Board
  ( Board (..),
    BoardDTO (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Board = Board
  { boardId :: Int,
    boardName :: Text,
    boardCategory :: Int
  }
  deriving (Show, Eq)

data BoardDTO = BoardDTO
  { id :: Int,
    name :: Text,
    category :: Int
  }
  deriving (Generic, ToJSON)
