{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Post
  ( Post (..),
    PostBody (..),
    PostDTO (..),
    postToDTO,
  )
where

import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)

data Post = Post
  { postId :: Int,
    postThreadId :: Int,
    postCreated :: UTCTime,
    postAuthor :: Maybe Text,
    postContent :: Text
  }
  deriving (Show, Eq)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field

data PostDTO = PostDTO
  { id :: Int,
    created :: UTCTime,
    author :: Maybe Text,
    content :: Text
  }
  deriving (Generic, ToJSON)

data PostBody = PostBody
  { bodyAuthor :: Maybe Text,
    bodyContent :: Text
  }
  deriving (Generic, Show)

instance FromJSON PostBody where
  parseJSON =
    Aeson.genericParseJSON
      defaultOptions
        { fieldLabelModifier = drop 4
        }

postToDTO :: Post -> PostDTO
postToDTO (Post pId _ pCreated pAuthor pContent) = PostDTO pId pCreated pAuthor pContent
