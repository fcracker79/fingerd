{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Domain.UserService where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged, managed)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection (Connection))
import Domain.User (User (username), UserName)
import Repository.Database (executeM)
import qualified Repository.UserRepository as R
import Service

ensureDatabase :: MonadManaged m => Pool Connection -> m ()
ensureDatabase pool = executeM pool R.createDatabase

getAllUserNames :: MonadManaged m => Pool Connection -> m [UserName]
getAllUserNames pool = executeM pool R.returnUsers

getUser :: MonadManaged m => Pool Connection -> UserName -> m (Maybe User)
getUser pool u = executeM pool $ \conn -> runMaybeT $ R.getUser conn u

responderQuery :: MonadManaged m => Pool Connection -> RespondG ServiceQueryK m
responderQuery pool = RespondG \case
  GetUsersReq -> GetUsersResp <$> getAllUserNames pool
  GetUserReq user -> GetUserResp <$> getUser pool user

responderEdit :: MonadManaged m => Pool Connection -> RespondG ServiceEditK m
responderEdit pool = RespondG \case
  SaveUserReq user -> executeM pool $ \conn -> do
    R.saveUser conn user
    pure SaveUserResp
  UpdateUserReq user -> pure UpdateUserResp
  DeleteUserReq userName -> pure DeleteUserResp
