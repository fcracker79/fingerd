module UserService where


import Data.Pool (Pool, withResource)
import Database.SQLite.Simple (Connection(Connection))
import qualified Repository.UserRepository as R
import Control.Monad.Managed ( managed, MonadManaged )
import Control.Monad.IO.Class (liftIO)
import Domain.User (UserName)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))

getAllUserNames :: MonadManaged m => Pool Connection -> m [UserName]
getAllUserNames p = do
    conn <- managed (withResource p)
    liftIO $ R.returnUsers conn


getUser :: MonadManaged m => Pool Connection -> UserName -> m (Maybe User)
getUser p u = do
    conn <- managed (withResource p)
    (liftIO . runMaybeT) $ R.getUser conn u 
