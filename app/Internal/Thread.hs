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
import Internal.Post.Handlers (getThreadPreview)

data Thread = Thread
  { threadId :: Int,
    threadBoardId :: Int
  }
  deriving (Show, Eq)

instance FromRow Thread where
  fromRow = Thread <$> field <*> field

newtype ThreadDTO = ThreadDTO
  { previewPosts :: [PostDTO]
  }
  deriving (Generic, ToJSON)

createThreadDTO :: AppEnv -> Thread -> IO ThreadDTO
createThreadDTO env (Thread tId _) = do
  posts <- getThreadPreview env tId
  return $ ThreadDTO posts

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
