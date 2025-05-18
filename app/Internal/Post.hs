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
import Data.Char (toLower)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)

data Post = Post
  { postId :: Int,
    postThreadId :: Int,
    postCreatedAt :: UTCTime,
    postSubject :: Maybe Text,
    postAuthor :: Maybe Text,
    postContent :: Text
  }
  deriving (Show, Eq)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field

data PostDTO = PostDTO
  { id :: Int,
    createdAt :: UTCTime,
    subject :: Maybe Text,
    author :: Maybe Text,
    content :: Text
  }
  deriving (Generic, ToJSON)

postToDTO :: Post -> PostDTO
postToDTO (Post pId _ pCreated pSubject pAuthor pContent) = PostDTO pId pCreated pSubject pAuthor pContent

data PostBody = PostBody
  { bodySubject :: Maybe Text,
    bodyAuthor :: Maybe Text,
    bodyContent :: Text
  }
  deriving (Generic, Show)

instance FromJSON PostBody where
  parseJSON =
    Aeson.genericParseJSON
      defaultOptions
        { fieldLabelModifier = \s ->
            let dropped = drop 4 s
             in toLower (head dropped) : tail dropped
        }
