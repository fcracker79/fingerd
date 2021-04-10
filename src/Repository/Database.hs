module Repository.Database where

import Data.Pool (Pool, createPool, withResource)
import Database.SQLite.Simple (Connection, open, close)
import Control.Monad.Managed (MonadManaged, managed, liftIO)

newPool :: FilePath -> IO (Pool Connection)
newPool fp = createPool (open fp) close 1 10 100

executeM :: MonadManaged m => Pool Connection -> (Connection -> IO b) -> m b
executeM p f = managed (withResource p) >>= liftIO . f 

