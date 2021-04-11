{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}

module Domain.UserService where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged, managed)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection (Connection))
import Domain.User (UserData (username), UserName, User)
import Repository.Database (executeM, WithPool, withPool)
import qualified Repository.UserRepository as R

ensureDatabase :: MonadManaged m => WithPool m ()
ensureDatabase = withPool R.createDatabase

getUsers :: MonadManaged m => WithPool m [UserName]
getUsers = withPool R.getUsers

    
getUser :: MonadManaged m => UserName -> WithPool m (Maybe User)
getUser user = withPool do \conn -> runMaybeT $ R.getUser conn user

saveUser :: MonadManaged m => UserData -> WithPool m UserName
saveUser user = withPool $ \conn -> do
    R.saveUser conn user

deleteUser :: MonadManaged m => UserName -> WithPool m UserName
deleteUser = error "not implemented"

updateUser :: MonadManaged m => User -> WithPool m UserName
updateUser = error "not implemented"


