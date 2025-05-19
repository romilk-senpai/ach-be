{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Media where

import Data.Aeson (ToJSON)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)

data MediaFile = MediaFile
  { mediaId :: Int,
    createdAt :: UTCTime,
    mediaPath :: String,
    mediaName :: String
  }
  deriving (Show, Eq)

instance FromRow MediaFile where
  fromRow = MediaFile <$> field <*> field <*> field <*> field

data MediaFileDTO = MediaFileDTO
  { id :: Int,
    path :: String,
    name :: String
  }
  deriving (Generic, ToJSON)

mediaFileToDTO :: MediaFile -> MediaFileDTO
mediaFileToDTO (MediaFile mId _ mPath mFileName) = MediaFileDTO mId mPath mFileName
