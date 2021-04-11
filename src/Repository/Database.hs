module Repository.Database where

import Data.Pool (Pool, createPool, withResource)
import Database.SQLite.Simple (Connection, open, close)
import Control.Monad.Managed (MonadManaged, managed, liftIO)
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT), MonadReader(ask) )

newtype Pooling m = Pooling {runPooling :: forall a. WithPool m a -> m a}
-- | open the SQLite database and create a connection pool
newPool :: FilePath -> IO (Pooling m)
newPool fp = do 
    pool <- createPool (open fp) close 1 10 100
    pure $ Pooling $ flip runReaderT pool  

-- | run a IO action picking a Connection from the pool
executeM :: MonadManaged m => Pool Connection -> (Connection -> IO b) -> m b
executeM pool action = managed (withResource pool) >>= liftIO . action 

type WithPool m = ReaderT (Pool Connection) m  

withPool :: MonadManaged m => (Connection -> IO b) -> WithPool m b
withPool action = ask >>= \pool -> executeM pool action 
