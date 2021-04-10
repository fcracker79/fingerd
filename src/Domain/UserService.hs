{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE DataKinds #-}
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

responderUser :: MonadManaged m => Pool Connection -> RespondG ViaUserK m
responderUser pool = RespondG \case
  GetUsersReq -> GetUsersResp <$> getAllUserNames pool
  GetUserReq user -> GetUserResp <$> getUser pool user

responderManagement :: MonadManaged m => Pool Connection -> RespondG ViaManagementK m
responderManagement pool = RespondG \case
  SaveUserReq user -> executeM pool $ \conn -> do
    R.saveUser conn user
    pure SaveUserResp 
  UpdateUserReq user  -> pure DeleteUserResp 
  DeleteUserReq userName ->  pure UpdateUserResp
