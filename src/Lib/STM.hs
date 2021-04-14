{-# LANGUAGE LambdaCase #-}

module Lib.STM where

import Control.Concurrent.STM
  ( STM
  , TVar
  , modifyTVar
  , newTVar
  , readTVar
  , retry
  , writeTVar
  )
import Control.Monad (guard)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), ViewL (EmptyL))

newtype Semaphore = Semaphore (TVar Bool)

newSemaphore :: STM Semaphore
newSemaphore = Semaphore <$> newTVar True

waitGreen :: Semaphore -> STM ()
waitGreen (Semaphore v) = do
  readTVar v >>= guard >> writeTVar v False

setGreen :: Semaphore -> STM ()
setGreen (Semaphore v) = writeTVar v True

newtype Baton a = Baton (TVar (Maybe a))

newEmptyBaton :: STM (Baton a)
newEmptyBaton = Baton <$> newTVar Nothing

waitBaton :: Baton a -> STM a
waitBaton (Baton v) = readTVar v >>= maybe retry pure

passBaton :: Baton a -> a -> STM ()
passBaton (Baton v) = writeTVar v . Just

newtype Queue a = Queue (TVar (Seq a))

newQueue :: STM (Queue a)
newQueue = Queue <$> newTVar mempty

exitQueue :: Queue a -> STM a
exitQueue (Queue v) =
  readTVar v >>= \case
    Empty -> retry
    x :<| rest -> writeTVar v rest >> pure x

enterQueue :: Queue a -> a -> STM ()
enterQueue (Queue v) x = modifyTVar v (:|> x)
