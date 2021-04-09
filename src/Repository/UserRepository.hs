{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Repository.UserRepository where


import Domain.User(User(..))
import Database.SQLite.Simple (FromRow(..), ToRow(..), field, Query, execute_, execute, query_, close, Connection, query, Only(..))
import Text.RawString.QQ ( r )
import Control.Exception (Exception, throwIO)
import Database.SQLite.Simple.Types ( Query, Null(..) )
import Data.Text (Text)


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



createUsers :: Query
createUsers = [r|
  CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE,
  shell TEXT, homeDirectory TEXT,
  realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"


data DuplicateData = DuplicateData deriving (Eq, Show)
instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)
createDatabase :: Connection -> IO ()
createDatabase conn = do
    execute_ conn createUsers
    user <- getUser conn userName
    case user of
      Nothing -> execute conn insertUser meRow
      _ -> return ()
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    where meRow :: UserRow
          meRow = (Null, userName, "/bin/zsh", "/home/callen", "Chris Allen", "555-123-4567")
          userName = "callen"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
    results <- query conn getUserQuery (Only username)
    case results of
      [] -> return $ Nothing
      [user] -> return $ Just user
      _ -> throwIO DuplicateData
