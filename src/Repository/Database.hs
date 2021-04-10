module Repository.Database where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (MonadManaged, managed, runManaged)
import Data.Pool (Pool, createPool, withResource)
import Database.SQLite.Simple (Connection, close, open, query)

newPool :: IO (Pool Connection)
newPool = createPool (open "finger.db") close 1 10 100

executeM :: MonadManaged m => Pool Connection -> (Connection -> IO b) -> m b
executeM p f = do
  conn <- managed (withResource p)
  liftIO $ f conn

