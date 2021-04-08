module User where


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


instance FromRow User where
    fromRow = User <$> field
               <*> field
               <*> field
               <*> field
               <*> field
               <*> field

instance ToRow User where
    toRow (User id_ username shell homeDir realName phone) =
        toRow (id_, username, shell, homeDir, realName, phone)