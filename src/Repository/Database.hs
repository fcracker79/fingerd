module Repository.Database where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (MonadManaged, managed, runManaged)
import Data.Pool (Pool, createPool, withResource)
import Database.SQLite.Simple (Connection, close, open, query)

pool :: IO (Pool Connection)
pool = createPool (open "finger.db") close 1 10 100

-- DO NOT USE. This is just an example
executeM :: MonadManaged m => (Connection -> IO b) -> m b
executeM f = do
  p <- liftIO pool
  conn <- managed (withResource p)
  liftIO $ f conn

execute :: (Connection -> IO ()) -> IO ()
execute f = runManaged $ executeM f
