{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Thread
  ( Thread (..),
    ThreadDTO (..),
    threadToDTO,
    createThreadDTO,
  )
where

import AppEnv (AppEnv)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)
import Internal.Post (PostDTO)
import Internal.Post.Handlers (getThreadPreview)

data Thread = Thread
  { threadId :: Int,
    threadBoardId :: Int,
    threadTitle :: Text,
    threadCreated :: UTCTime,
    threadAuthor :: Maybe Text
  }
  deriving (Show, Eq)

instance FromRow Thread where
  fromRow = Thread <$> field <*> field <*> field <*> field <*> field

data ThreadDTO = ThreadDTO
  { id :: Int,
    title :: Text,
    created :: UTCTime,
    author :: Maybe Text,
    previewPosts :: [PostDTO]
  }
  deriving (Generic, ToJSON)

createThreadDTO :: AppEnv -> Thread -> IO ThreadDTO
createThreadDTO env (Thread tId _ tTitle tCreated tAuthor) = do
  posts <- getThreadPreview env tId
  return $ ThreadDTO tId tTitle tCreated tAuthor posts

threadToDTO :: Thread -> ThreadDTO
threadToDTO (Thread tId _ tTitle tCreated tAuthor) = do
  ThreadDTO tId tTitle tCreated tAuthor []
