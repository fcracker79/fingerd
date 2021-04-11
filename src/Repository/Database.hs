module Repository.Database where

import Data.Pool (Pool, createPool, withResource)
import Database.SQLite.Simple (Connection, open, close)
import Control.Monad.Managed (MonadManaged, managed, liftIO)

-- | open the SQLite database and create a connection pool
newPool :: FilePath -> IO (Pool Connection)
newPool fp = createPool (open fp) close 1 10 100

-- | run a IO action picking a Connection from the pool
executeM :: MonadManaged m => Pool Connection -> (Connection -> IO b) -> m b
executeM pool action = managed (withResource pool) >>= liftIO . action 

