{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.BoardInfo
  ( BoardInfo (..),
    BoardInfoDTO (..),
    boardInfoToDTO,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)

data BoardInfo = BoardInfo
  { boardInfoId :: Int,
    boardInfoName :: Text,
    boardInfoCategoryId :: Int,
    boardInfoCategoryName :: String,
    boardInfoSlug :: Text
  }
  deriving (Show, Eq)

instance FromRow BoardInfo where
  fromRow = BoardInfo <$> field <*> field <*> field <*> field <*> field

data BoardInfoDTO = BoardInfoDTO
  { id :: Int,
    name :: Text,
    categoryName :: String,
    slug :: Text
  }
  deriving (Generic, ToJSON)

boardInfoToDTO :: BoardInfo -> BoardInfoDTO
boardInfoToDTO (BoardInfo bId bName _ bCatName bSlug) = BoardInfoDTO bId bName bCatName bSlug
