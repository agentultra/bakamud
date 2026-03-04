module Bakamud.Avatar where

import Data.Int
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB

data Avatar
  = Avatar
  { _avatarId           :: Int64
  , _avatarName         :: Text
  , _avatarConnectionId :: Int64 -- TODO: Change this to a UserId once auth works better
  }
  deriving (Eq, Show)

instance DB.ToRow Avatar where
  toRow (Avatar _id name connectionId) = DB.toRow (_id, name, connectionId)

instance DB.FromRow Avatar where
  fromRow = Avatar <$> DB.field <*> DB.field <*> DB.field
