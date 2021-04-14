{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleContexts #-}
module Database where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception (Exception, bracket, throw)
import Control.Monad.Managed (Managed, MonadManaged, liftIO, managed)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader
  ( MonadIO (liftIO)
  , MonadReader (ask)
  , ReaderT (ReaderT, runReaderT)
  )
import Data.Functor (($>))
import Data.Text (Text, singleton)
import Database.SQLite.Simple
  ( Connection
  , FromRow (..)
  , Only (Only)
  , Query
  , ToRow (..)
  , close
  , execute
  , field
  , open
  , query
  , withConnection
  )
import Database.SQLite.Simple.QQ (sql)
import User (User (User, userData), UserData (..), UserName)
import Lib.SQLiteSafe


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

type WithDB = ReaderT SQLiteSafe IO

withDB :: SQLiteSafe -> (forall  a. WithDB a -> IO a)
withDB = flip runReaderT 

queryM :: (ToRow q, FromRow r) => Query -> q -> WithDB [r]
queryM q x = ask >>= \SQLiteSafe {..} -> liftIO $ query q x

executeM :: (ToRow q) => Query -> q -> WithDB ()
executeM q x = ask >>= \SQLiteSafe {..} -> liftIO $ execute q x

executeM_ ::  Query -> WithDB ()
executeM_ q = executeM q ()

data DuplicateData = DuplicateData deriving (Eq, Show, Exception)

createDatabase :: WithDB ()
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

getUser ::  UserName -> WithDB (Maybe User)
getUser username = do
  results <- queryM "SELECT * from users where username = ?" $ Only username
  case results of
    [] -> pure Nothing
    [user] -> pure $ Just user
    _ -> throw DuplicateData

getUsers :: WithDB [UserName]
getUsers = fmap (username . userData) <$> queryM "SELECT * from users" ()

saveUser ::  UserData -> WithDB Bool
saveUser userData@UserData {..} = do
  existingUser <- getUser username
  case existingUser of
    Nothing -> executeM "INSERT INTO users VALUES (null, ?, ?, ?, ?, ?)" userData $> True
    _ -> pure False

updateUser :: UserData -> WithDB Bool
updateUser UserData {..} = do
  existingUser <- getUser username
  case existingUser of
    Nothing -> pure False
    Just _ ->
      executeM
        "UPDATE users SET shell = ?, homeDirectory = ?, realName = ?, phone = ? WHERE username = ?"
        (shell, homeDirectory, realName, phone, username)
        $> True

deleteUser :: UserName -> WithDB Bool
deleteUser userName = do
  existingUser <- getUser userName
  case existingUser of
    Nothing -> pure False
    Just _ ->
      executeM
        "DELETE FROM users WHERE username = ?"
        (Only userName)
        $> True
