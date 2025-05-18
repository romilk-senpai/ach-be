{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Thread
  ( Thread (..),
    ThreadDTO (..),
    ThreadBody (..),
    createThreadDTO,
  )
where

import AppEnv (AppEnv)
import Data.Aeson (Options (fieldLabelModifier), ToJSON, defaultOptions)
import Data.Aeson.Types (FromJSON (parseJSON))
import qualified Data.Aeson.Types as Aeson
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)
import Internal.Post (PostBody, PostDTO)
import qualified Internal.Post.Storage as Storage

data Thread = Thread
  { threadId :: Int,
    threadBoardId :: Int
  }
  deriving (Show, Eq)

instance FromRow Thread where
  fromRow = Thread <$> field <*> field

data ThreadDTO = ThreadDTO
  { opPost :: PostDTO,
    lastReplies :: [PostDTO]
  }
  deriving (Generic, ToJSON)

createThreadDTO :: AppEnv -> Thread -> IO ThreadDTO
createThreadDTO env (Thread tId _) = do
  topPost <- Storage.getOpPost env tId
  posts <- Storage.getThreadLastReplies env tId
  return $ ThreadDTO topPost posts

newtype ThreadBody = ThreadBody
  { bodyOpPost :: PostBody
  }
  deriving (Generic, Show)

instance FromJSON ThreadBody where
  parseJSON =
    Aeson.genericParseJSON
      defaultOptions
        { fieldLabelModifier = drop 4
        }
