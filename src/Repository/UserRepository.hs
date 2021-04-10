{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE ViewPatterns #-}
module Repository.UserRepository where

import Control.Applicative (empty)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, FromRow (..), Only (..), Query, ToRow (..), close, execute, execute_, field, query, query_)
import Database.SQLite.Simple.Types (Null (..), Query)
import Domain.User (User (..), UserName)
import Text.RawString.QQ (r)

instance FromRow User where
  fromRow =
    User <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers =
  [r|
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

type UserRow = (Null, T.Text, T.Text, T.Text, T.Text, T.Text)

createDatabase :: Connection -> IO ()
createDatabase conn = do
  execute_ conn createUsers
  user <- runMaybeT $ getUser conn userName
  case user of
    Nothing -> execute conn insertUser meRow
    _ -> return ()
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  where
    meRow :: UserRow
    meRow = (Null, userName, "/bin/zsh", "/home/callen", "Chris Allen", "555-123-4567")
    userName = "callen"

getUser :: Connection -> UserName -> MaybeT IO User
getUser conn (T.strip -> username) = do
  results <- liftIO (query conn getUserQuery (Only username))
  case results of
    [] -> empty
    [user] -> return user
    _ -> (liftIO . throwIO) DuplicateData

returnUsers :: Connection -> IO [UserName]
returnUsers dbConn = do
  rows <- query_ dbConn allUsers
  return $ map username rows
