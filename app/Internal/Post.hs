{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Post
  ( Post (..),
    PostDTO (..),
    postToDTO,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)

data Post = Post
  { postId :: Int,
    postTopicId :: Int,
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

postToDTO :: Post -> PostDTO
postToDTO (Post pId _ pCreated pAuthor pContent) = PostDTO pId pCreated pAuthor pContent
