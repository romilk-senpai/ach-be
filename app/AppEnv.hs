module AppEnv (AppEnv (..)) where

import Database.PostgreSQL.Simple (Connection)

data AppEnv = AppEnv
  { dbConn :: Connection
  }
