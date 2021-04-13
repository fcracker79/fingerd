{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AccessControl where

import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM
  ( atomically
  , newEmptyTMVar
  , newTChanIO
  , newTMVarIO
  , putTMVar
  , readTChan
  , takeTMVar
  , writeTChan
  )
import Control.Exception (bracket)
import Control.Monad.Managed (MonadIO (..), MonadManaged, managed)
import Control.Monad.Reader (MonadReader, ReaderT (..), forever)
import Data.Functor (($>))

-- | intercept a (r -> m a) call
newtype AccessController r m = AccessController
  { controllingAccess :: forall a. (r -> m a) -> m a
  }

-- | specialize 'm' to AccessControl r m
type WithAccessController r m = AccessController r (AccessControl r m)

-- | a reader that has a WithAccessController in the environment
newtype AccessControl r m a = AccessControl
  { hasAccessControl :: ReaderT (WithAccessController r m) m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AccessController r (AccessControl r m))
    , MonadIO
    , MonadManaged
    )

type HasAccessControl r m = MonadReader (AccessController r m) m

runAccessControl
  :: WithAccessController r m -- ^ controller for 'r' to go in the environemt
  -> AccessControl r m a -- ^ action that needs controlled access to  'r'
  -> m a
runAccessControl env = flip runReaderT env . hasAccessControl

-- | create an AccessController that disregard access ordering but
-- ensure only one access at the time to 'r' is allowed
singleThreading
  :: (MonadIO t, MonadManaged m)
  => r -- ^ resource to protect from race
  -> t (AccessController r m)
singleThreading conn = do
  store <- liftIO $ newTMVarIO conn
  let restore = atomically . putTMVar store
  pure $ AccessController \action -> do
    conn <- managed $ bracket
      do atomically $ takeTMVar store
      do restore
    result <- action conn
    liftIO (restore conn) $> result

-- | create an AccessController that take into account access ordering and
-- ensure only one access at the time to 'r' is allowed
fairSingleThreading :: MonadManaged m => r -> m (WithAccessController r m)
fairSingleThreading conn = do
  store <- liftIO $ newTMVarIO conn
  queue <- liftIO newTChanIO
  server <- managed $
    withAsync $
      forever $
        atomically $ do
          waiting <- readTChan queue
          takeTMVar store >>= putTMVar waiting
  liftIO $ link server
  let restore = atomically . putTMVar store
  pure $ AccessController \action -> do
    waiting <- liftIO $
      atomically $ do
        waiting <- newEmptyTMVar
        writeTChan queue waiting
        pure waiting
    conn <- managed $ bracket
      do atomically $ takeTMVar waiting
      do restore
    result <- action conn
    liftIO (restore conn) $> result
