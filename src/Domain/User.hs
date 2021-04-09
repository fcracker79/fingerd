module Domain.User where


import Data.Text (Text)
import Database.SQLite.Simple (FromRow, fromRow, field)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
data User = User {
    userId :: Integer
    , username :: Text
    , shell :: Text
    , homeDirectory :: Text
    , realName :: Text
    , phone :: Text
} deriving (Eq, Show)
