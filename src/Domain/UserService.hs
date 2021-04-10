{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Domain.UserService where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged, managed)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Controller
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection (Connection))
import Domain.User (User (username), UserName)
import Repository.Database (executeM)
import qualified Repository.UserRepository as R

ensureDatabase :: MonadManaged m => Pool Connection -> m ()
ensureDatabase pool = executeM pool R.createDatabase

getAllUserNames :: MonadManaged m => Pool Connection -> m [UserName]
getAllUserNames pool = executeM pool R.returnUsers

getUser :: MonadManaged m => Pool Connection -> UserName -> m (Maybe User)
getUser pool u = executeM pool $ \conn -> runMaybeT $ R.getUser conn u

responderQuery :: MonadManaged m => Pool Connection -> Respond ServiceQueryK m
responderQuery pool = Respond \case
  GetUsersReq -> GetUsersResp <$> getAllUserNames pool
  GetUserReq user -> GetUserResp <$> getUser pool user

responderEdit :: MonadManaged m => Pool Connection -> Respond ServiceEditK m
responderEdit pool = Respond \case
  SaveUserReq user -> executeM pool $ \conn -> do
    R.saveUser conn user
    pure SaveUserResp
  UpdateUserReq user -> pure UpdateUserResp
  DeleteUserReq userName -> pure DeleteUserResp
