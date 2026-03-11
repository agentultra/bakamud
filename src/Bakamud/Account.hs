module Bakamud.Account where

import Data.Int
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.Internal as DB
import qualified Database.SQLite.Simple.Ok as DB
import qualified Database.SQLite.Simple.ToField as DB

newtype Username = Username Text
  deriving (Eq, Ord, Show)

instance DB.ToField Username where
  toField (Username username) = DB.toField username

instance DB.FromField Username where
  fromField (DB.Field (DB.SQLText username) _) = DB.Ok $ Username username
  fromField f = DB.returnError DB.ConversionFailed f "Unable to parse username"

newtype Password = Password { getPassword :: Text }
  deriving (Eq)

instance DB.ToField Password where
  toField (Password pass) = DB.toField pass

instance DB.FromField Password where
  fromField (DB.Field (DB.SQLText pass) _) = DB.Ok $ Password pass
  fromField f = DB.returnError DB.ConversionFailed f "Unable to parse password"

instance Show Password where
  show (Password _) = "(Password ***)"

data Account
  = Account
  { _accountId       :: Int64
  , _accountUserName :: Username
  , _accountUserPass :: Password
  }
  deriving (Eq, Show)

instance DB.ToRow Account where
  toRow (Account userId username password) = DB.toRow (userId, username, password)

instance DB.FromRow Account where
  fromRow = Account <$> DB.field <*> DB.field <*> DB.field
