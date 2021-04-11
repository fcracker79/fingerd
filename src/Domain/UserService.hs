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
import Domain.User (UserData (username), UserName, User)
import Repository.Database (executeM)
import qualified Repository.UserRepository as R

ensureDatabase :: MonadManaged m => Pool Connection -> m ()
ensureDatabase pool = executeM pool R.createDatabase

getUsers :: MonadManaged m => Pool Connection -> m [UserName]
getUsers pool = executeM pool R.getUsers

responderQuery :: MonadManaged m => Pool Connection -> Respond ServiceQueryType m
responderQuery pool = Respond \case
  GetUsersReq -> GetUsersResp <$> getUsers pool
  GetUserReq user -> GetUserResp <$> getUser pool user 
    
getUser :: MonadManaged m => Pool Connection -> UserName -> m (Maybe User)
getUser pool user = executeM
      pool
      do \conn -> runMaybeT $ R.getUser conn user

responderEdit :: MonadManaged m => Pool Connection -> Respond ServiceEditType m
responderEdit pool = Respond \case
  SaveUserReq user -> executeM pool $ \conn -> do
    R.saveUser conn user
    pure $ SaveUserResp True 
  UpdateUserReq user -> pure $ UpdateUserResp False
  DeleteUserReq userName -> pure $ DeleteUserResp False 
