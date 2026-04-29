module Bakamud.Session where

import Bakamud.Account
import Bakamud.Avatar
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB

data Session
  = Session
  { _sessionAccountId    :: Maybe AccountId
  , _sessionAvatarId     :: Maybe AvatarId
  , _sessionAvatarRoomId :: Maybe Text
  }
  deriving (Eq, Show)

instance DB.ToRow Session where
  toRow (Session accountId avatarId avatarRoomId) = DB.toRow (accountId, avatarId, avatarRoomId)

instance DB.FromRow Session where
  fromRow = Session <$> DB.field <*> DB.field <*> DB.field
