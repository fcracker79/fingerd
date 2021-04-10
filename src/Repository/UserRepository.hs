{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Repository.UserRepository where

import Control.Applicative (empty)
import Control.Exception (Exception, throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, FromRow (..), Only (..), Query, ToRow (..), close, execute, execute_, field, query, query_)
import Database.SQLite.Simple.Types (Null (..), Query)
import Domain.User (User (..), UserData (..), UserName)
import Text.RawString.QQ (r)

instance FromRow User where
  fromRow =
    User <$> field
      <*> do
        UserData <$> field
          <*> field
          <*> field
          <*> field
          <*> field

instance ToRow User where
  toRow (User id_ (UserData username shell homeDir realName phone)) =
    toRow
      ( id_
      , username
      , shell
      , homeDir
      , realName
      , phone
      )

createUsers :: Query
createUsers =
  [r|
  CREATE TABLE IF NOT EXISTS users
    ( id INTEGER PRIMARY KEY AUTOINCREMENT
    , username TEXT UNIQUE
    , shell TEXT
    , homeDirectory TEXT
    , realName TEXT
    , phone TEXT
    )
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Exception)

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
getUser conn username = do
  results <- liftIO $ query conn getUserQuery $ Only username
  case results of
    [] -> empty
    [user] -> pure user
    _ -> throw DuplicateData

getUsers :: Connection -> IO [UserName]
getUsers dbConn = fmap (username . userData) <$> query_ dbConn allUsers

saveUser :: Connection -> UserData -> IO UserName
saveUser dbConn user@UserData {..} = do
  existingUser <- runMaybeT $ getUser dbConn username
  -- TODO there must be a better way to do thid
  case existingUser of
    Nothing -> do
      execute dbConn insertUser (Null, username, shell, homeDirectory, realName, phone)
      return username
    _ -> return username
