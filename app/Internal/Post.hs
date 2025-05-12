{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Post
  ( Post (..),
    PostDTO (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Post = Post
  { postId :: Int,
    postTopicId :: Int,
    postCreated :: UTCTime,
    postAuthor :: Maybe Text,
    postContent :: Text
  }
  deriving (Show, Eq)

data PostDTO = PostDTO
  { author :: Maybe Text,
    content :: Text,
    created :: UTCTime
  }
  deriving (Generic, ToJSON)
