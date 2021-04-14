{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Lib.SQLiteSafe where

import Control.Concurrent.Async (async, link)
import Control.Concurrent.STM (atomically)
import Control.Exception (throwIO)
import Control.Monad (forever, join, when)
import Control.Monad.Catch (SomeException, bracket, catchAll)
import Data.Functor (($>))
import Data.Traversable (forM)
import Database.SQLite.Simple (Connection, FromRow, Query, ToRow, close, open)
import qualified Database.SQLite.Simple as DB
import Lib.STM
  ( enterQueue
  , exitQueue
  , newEmptyBaton
  , newQueue
  , newSemaphore
  , passBaton
  , setGreen
  , waitBaton
  , waitGreen
  )

-- | control baton passing
data SQLiteSafe = SQLiteSafe
  { query
    :: forall q r.
         (ToRow q, FromRow r)
         => Query
         -> q
         -> IO [r]
  , execute
    :: forall q.
         ToRow q
         => Query
         -> q
         -> IO ()
  }

-- | create an Access that respect the access order and
-- ensure only one runner at the time has his baton
-- and the baton is coming from previous runner
sqliteSafe
  :: Connection -- ^ resource to protect from races
  -> (SomeException -> IO ()) -- ^ handler for exceptions
  -> IO SQLiteSafe
sqliteSafe conn handle = do
  sem <- atomically newSemaphore
  queue <- atomically newQueue
  server <- async $
    forever $
      atomically $ do
        waiting <- exitQueue queue
        waitGreen sem >>= passBaton waiting
  link server
  let restore = atomically $ setGreen sem
  pure $ SQLiteSafe
    do DB.query conn
    do
      \q p -> do
        join $
          atomically $ do
            waiting <- newEmptyBaton
            enterQueue queue waiting
                $> atomically (waitBaton waiting)
        catchAll
          do DB.execute conn q p *>  restore
          do \e -> restore *> handle e

openSqliteSafe
  :: FilePath
  -> (SQLiteSafe -> IO r)
  -> IO r
openSqliteSafe path action = bracket
  do open path
  do close
  do
    \conn -> do
      safe <- sqliteSafe conn throwIO
      result <- action safe
      close conn $> result
