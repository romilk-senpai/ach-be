{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Topic
  ( Topic (..),
    TopicDTO (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Topic = Topic
  { topicId :: Int,
    topicBoardId :: Int,
    topicTitle :: Text,
    topicCreated :: UTCTime,
    topicAuthor :: Maybe Text
  }
  deriving (Show, Eq)

data TopicDTO = TopicDTO
  { id :: Int,
    title :: Text,
    created :: UTCTime,
    author :: Maybe Text
  }
  deriving (Generic, ToJSON)
