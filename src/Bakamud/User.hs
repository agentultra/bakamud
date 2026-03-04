module Bakamud.User where

import Data.Int
import qualified Database.SQLite.Simple as DB

newtype User = User { _userId :: Int64 }

instance DB.ToRow User where
  toRow (User userId) = DB.toRow (DB.Only userId)

instance DB.FromRow User where
  fromRow = User <$> DB.field
