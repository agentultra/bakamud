module Bakamud.Avatar where

import Bakamud.Account
import Data.Int
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.Internal as DB
import qualified Database.SQLite.Simple.Ok as DB
import qualified Database.SQLite.Simple.ToField as DB

newtype AvatarId = AvatarId Int64
  deriving (Eq, Show)

instance DB.ToField AvatarId where
  toField (AvatarId avatarId) = DB.toField avatarId

instance DB.FromField AvatarId where
  fromField (DB.Field (DB.SQLInteger avatarId) _) = DB.Ok $ AvatarId avatarId
  fromField f = DB.returnError DB.ConversionFailed f "Unable to parse password"

data Avatar
  = Avatar
  { _avatarId        :: AvatarId
  , _avatarName      :: Text
  , _avatarAccountId :: AccountId
  }
  deriving (Eq, Show)

instance DB.ToRow Avatar where
  toRow (Avatar _id name accountId) = DB.toRow (_id, accountId, name)

instance DB.FromRow Avatar where
  fromRow = Avatar <$> DB.field <*> DB.field <*> DB.field
