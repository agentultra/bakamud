module Bakamud.User where

import Data.Int
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB

data User
  = User
  { _userId   :: Int64
  , _userName :: Text
  }
  deriving (Eq, Show)

instance DB.ToRow User where
  toRow (User userId userName) = DB.toRow (userId, userName)

instance DB.FromRow User where
  fromRow = User <$> DB.field <*> DB.field
