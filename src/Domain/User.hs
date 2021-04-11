module Domain.User where

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

renderUser :: Maybe User -> ByteString
renderUser Nothing = "No such user"
renderUser (Just (User _ (UserData username shell homeDir realName _))) =
  BS.concat
    [  "Login: "
    , e username
    , "\t\t\t\t"
    , "Name: "
    , e realName
    , "\n"
    , "Directory: "
    , e homeDir
    , "\t\t\t"
    , "Shell: "
    , e shell
    , "\n"
    ]
  where
    e = encodeUtf8

renderUsers :: [UserName] -> ByteString
renderUsers userNames = encodeUtf8 $ T.concat $ intersperse "\n" userNames
