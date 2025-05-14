{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Topic
  ( Topic (..),
    TopicDTO (..),
    topicToDTO,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)

data Topic = Topic
  { topicId :: Int,
    topicBoardId :: Int,
    topicTitle :: Text,
    topicCreated :: UTCTime,
    topicAuthor :: Maybe Text
  }
  deriving (Show, Eq)

instance FromRow Topic where
  fromRow = Topic <$> field <*> field <*> field <*> field <*> field

data TopicDTO = TopicDTO
  { id :: Int,
    title :: Text,
    created :: UTCTime,
    author :: Maybe Text
  }
  deriving (Generic, ToJSON)

topicToDTO :: Topic -> TopicDTO
topicToDTO (Topic tId _ tTitle tCreated tAuthor) = TopicDTO tId tTitle tCreated tAuthor
