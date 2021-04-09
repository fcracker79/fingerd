module Repository.Database where


import Data.Pool (createPool, Pool, withResource)
import Database.SQLite.Simple (open, close, Connection, query)
import Control.Monad.Managed (MonadManaged, managed, runManaged )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

pool :: IO (Pool Connection)
pool = createPool (open "finger.db") close 1 10 100


-- DO NOT USE. This is just an example
executeM :: Control.Monad.Managed.MonadManaged m => (Connection -> IO b) -> m b
executeM f = do
    p <- liftIO pool
    conn <- managed (withResource p)
    liftIO $ f conn


execute :: (Connection -> IO ()) -> IO ()
execute f = runManaged $ executeM f