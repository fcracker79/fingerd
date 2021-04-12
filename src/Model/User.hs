module Model.User where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8 )
import Database.SQLite.Simple (FromRow, field, fromRow)
import Database.SQLite.Simple.ToRow (ToRow, toRow)

type UserName = Text

data User = User
  { userId :: Integer
  , userData :: UserData
  }
  deriving (Eq, Show)
data UserData = UserData 

  { username :: UserName
  , shell :: Text
  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
  }
  deriving (Eq, Show)


