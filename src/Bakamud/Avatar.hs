module Bakamud.Avatar where

import Data.Int
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB

data Avatar
  = Avatar
  { _avatarId        :: Int64
  , _avatarName      :: Text
  , _avatarAccountId :: Int64
  }
  deriving (Eq, Show)

instance DB.ToRow Avatar where
  toRow (Avatar _id name accountId) = DB.toRow (_id, accountId, name)

instance DB.FromRow Avatar where
  fromRow = Avatar <$> DB.field <*> DB.field <*> DB.field
