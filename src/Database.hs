{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

import Control.Applicative
import Control.Exception (Exception, bracket, throw)
import Control.Monad.Managed (MonadManaged, liftIO, managed)
import Control.Monad.Reader
  ( MonadIO (liftIO)
  , MonadReader (ask)
  , ReaderT (runReaderT)
  )
import Control.Monad.Trans.Maybe
import Data.Functor (($>))
import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.Types
import Model.User

-- | open the SQLite database and create a connection pool
newDB :: MonadManaged m => FilePath -> m Connection
newDB fp = managed $ bracket
  do open fp
  do close

type HasConnection m = ReaderT Connection m

instance FromRow UserData where
  fromRow =
    UserData <$> field
      <*> field
      <*> field
      <*> field
      <*> field

instance FromRow User where
  fromRow =
    User <$> field
      <*> fromRow

instance ToRow UserData where 
  toRow (UserData username shell homeDir realName phone) =
    toRow
      ( username
      , shell
      , homeDir
      , realName
      , phone
      )
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

queryM :: (MonadReader Connection m, MonadIO m, ToRow q, FromRow r) => Query -> q -> m [r]
queryM q x = ask >>= \conn -> liftIO $ query conn q x

executeM :: (MonadReader Connection m, MonadIO m, ToRow q) => Query -> q -> m ()
executeM q x = ask >>= \conn -> liftIO $ execute conn q x

executeM_ :: (MonadReader Connection m, MonadIO m) => Query -> m ()
executeM_ q = executeM q ()
data DuplicateData = DuplicateData deriving (Eq, Show, Exception)

createDatabase :: HasConnection IO ()
createDatabase =
    executeM_
      [sql|
        CREATE TABLE IF NOT EXISTS users
            ( id INTEGER PRIMARY KEY AUTOINCREMENT
            , username TEXT UNIQUE
            , shell TEXT
            , homeDirectory TEXT
            , realName TEXT
            , phone TEXT
            )
        |]

getUser :: UserName -> HasConnection IO (Maybe User)
getUser username = do
  results <- queryM "SELECT * from users where username = ?" $ Only username
  case results of
    [] -> pure Nothing
    [user] -> pure $ Just user
    _ -> throw DuplicateData

getUsers :: HasConnection IO [UserName]
getUsers = fmap (username . userData) <$> queryM "SELECT * from users" ()

saveUser :: UserData -> HasConnection IO Bool
saveUser userData@UserData {..} = do
  existingUser <- getUser username 
  case existingUser of
    Nothing -> executeM "INSERT INTO users VALUES (null, ?, ?, ?, ?, ?)" userData $> True
    _ -> pure False

updateUser :: UserData -> HasConnection IO Bool
updateUser UserData {..} = do
  existingUser <- getUser username
  case existingUser of
    Nothing -> pure False
    Just _ ->
      executeM
        "UPDATE users SET shell = ?, homeDirectory = ?, realName = ?, phone = ? WHERE username = ?"
        (shell, homeDirectory, realName, phone, username)
        $> True

deleteUser :: UserName -> HasConnection IO Bool
deleteUser userName = do
  existingUser <- getUser userName
  case existingUser of
    Nothing -> pure False
    Just _ ->
      executeM
        "DELETE FROM users WHERE username = ?"
        (Only userName)
        $> True
