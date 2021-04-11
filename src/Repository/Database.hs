{-# LANGUAGE BlockArguments #-}
module Repository.Database where

import Data.Pool (Pool, createPool, withResource, destroyAllResources)
import Database.SQLite.Simple (Connection, open, close)
import Control.Monad.Managed (MonadManaged, managed, liftIO)
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT), MonadReader(ask) )
import Control.Exception (bracket)

newtype Pooling m = Pooling {runPooling :: forall a. WithPool m a -> m a}
-- | open the SQLite database and create a connection pool
newPool :: MonadManaged m => FilePath -> m (Pooling m)
newPool fp = do 
    pool <- managed $ bracket 
        do createPool (open fp) close 1 10 100
        do destroyAllResources 
    pure $ Pooling $ flip runReaderT pool  

-- | run a IO action picking a Connection from the pool
executeM :: MonadManaged m => Pool Connection -> (Connection -> IO b) -> m b
executeM pool action = managed (withResource pool) >>= liftIO . action 

type WithPool m = ReaderT (Pool Connection) m  

withPool :: MonadManaged m => (Connection -> IO b) -> WithPool m b
withPool action = ask >>= \pool -> executeM pool action 
