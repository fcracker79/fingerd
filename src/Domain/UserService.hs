module Domain.UserService where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged, managed)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection (Connection))
import Domain.User (User, UserName)
import qualified Repository.UserRepository as R
import Service (Request (ReqUser, ReqUsers), Respond, renderUsers, renderUser)

getAllUserNames :: MonadManaged m => Pool Connection -> m [UserName]
getAllUserNames p = do
  conn <- managed (withResource p)
  liftIO $ R.returnUsers conn

getUser :: MonadManaged m => Pool Connection -> UserName -> m (Maybe User)
getUser p u = do
  conn <- managed (withResource p)
  (liftIO . runMaybeT) $ R.getUser conn u

responder :: MonadManaged m => Pool Connection -> Respond m
responder pool ReqUsers = renderUsers <$> getAllUserNames pool
responder pool (ReqUser user) = renderUser <$> getUser pool user


