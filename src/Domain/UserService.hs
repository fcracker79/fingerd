module Domain.UserService where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged, managed)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection (Connection))
import Domain.User (User (username), UserName)
import qualified Repository.UserRepository as R
import Service (Request (ReqUser, ReqUsers), Respond, renderUser, renderUsers, EditRequest (SaveUser, UpdateUser, DeleteUser), EditRespond)
import Repository.Database (executeM)

ensureDatabase :: MonadManaged m => Pool Connection -> m ()
ensureDatabase p = executeM p R.createDatabase 

getAllUserNames :: MonadManaged m => Pool Connection -> m [UserName]
getAllUserNames p = executeM p  R.returnUsers 

getUser :: MonadManaged m => Pool Connection -> UserName -> m (Maybe User)
getUser p u = executeM p $ \conn -> runMaybeT $ R.getUser conn u

responder :: MonadManaged m => Pool Connection -> Respond m
responder pool ReqUsers = renderUsers <$> getAllUserNames pool
responder pool (ReqUser user) = renderUser <$> getUser pool user

responderForEdit :: MonadManaged m => Pool Connection -> EditRespond m
responderForEdit pool (SaveUser user) = executeM pool $ \conn -> do
    R.saveUser conn user
    return "OK" 
responderForEdit pool (UpdateUser user) = return "TODO UPDATE"
responderForEdit pool (DeleteUser userName) = return "TODO DELETE"
