{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Lib.SQLiteSafe where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link, wait, withAsync)
import Control.Concurrent.STM
import Control.Monad (forever, when)
import Control.Monad.Catch (SomeException, catchAll)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Traversable (forM)
import Database.SQLite.Simple (Connection, FromRow, Query, ToRow, open, close)
import qualified Database.SQLite.Simple as DB
import System.Random (Random (randomIO, randomRIO))
import Test.HUnit ((@?=))
import Control.Monad.Catch.Pure (bracket)
import Control.Exception (throwIO)

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
  store <- newTMVarIO conn
  queue <- newTChanIO
  server <- async $
    forever $
      atomically $ do
        waiting <- readTChan queue
        takeTMVar store >>= putTMVar waiting
  link server
  let restore f = atomically . f store
  pure $ SQLiteSafe
    do DB.query conn
    do
      \q p -> do
        waiting <-
          atomically $ do
            waiting <- newEmptyTMVar
            writeTChan queue waiting
            pure waiting
        conn <- atomically $ takeTMVar waiting
        catchAll
          do
            DB.execute conn q p
            restore putTMVar conn
          do \e -> restore tryPutTMVar conn >> handle e

openSqliteSafe
  :: FilePath
  -> (SQLiteSafe -> IO r)
  -> IO r
openSqliteSafe path action  = bracket 
  do open path 
  do close 
  do \conn  -> do 
      safe <- sqliteSafe conn throwIO 
      result <- action safe
      close conn $> result 
