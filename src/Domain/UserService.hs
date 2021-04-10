{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

module Domain.UserService where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged, managed)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection (Connection))
import Domain.User (User, UserName)
import Repository.Database (executeM)
import qualified Repository.UserRepository as R
import Service
  ( RequestG (UserReq, UsersReq)
  , RespondG (..)
  , ResponseG (UserResp, UsersResp)
  )

ensureDatabase :: MonadManaged m => Pool Connection -> m ()
ensureDatabase p = executeM p R.createDatabase

getAllUserNames :: MonadManaged m => Pool Connection -> m [UserName]
getAllUserNames p = executeM p R.returnUsers

getUser :: MonadManaged m => Pool Connection -> UserName -> m (Maybe User)
getUser p u = executeM p $ \conn -> runMaybeT $ R.getUser conn u

responder :: MonadManaged m => Pool Connection -> RespondG m
responder pool = RespondG \case
  UsersReq -> UsersResp <$> getAllUserNames pool
  UserReq user -> UserResp <$> getUser pool user
