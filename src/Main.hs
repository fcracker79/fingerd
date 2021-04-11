{-# LANGUAGE BlockArguments #-}

{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Control.Concurrent.Thread.Group as TG
import Control.Monad.Catch
import Control.Monad.Managed (Managed, runManaged)
import Control.Monad.Reader
import GadtController (hoistRespond, parseEdit, parseQuery, responderQuery, responderEdit)
import Repository.Database (Pooling (Pooling), WithPool, newPool, runPooling)
import Server (ServerHandler, gadtServerHandler, server, existentialServerHandler)
import Network.Socket (Socket)
import ExistentialController (queryController, hoistController, editController)
import Control.Monad.Morph (hoist)
import Domain.UserService (ensureDatabase)


main :: IO ()
main = gadtMain 

mainWith
  :: (Pooling Managed -> ServerHandler) -- ^ 
  -> (Pooling Managed -> ServerHandler) -- ^ 
  -> IO ()
mainWith serverHandlerQuery serverHandlerEdit = do
  pooling <- newPool "finger.db"
  runManaged $ runPooling pooling ensureDatabase
  group <- TG.new
  TG.forkIO group $
    catchAll
      do
        server "79" $ serverHandlerQuery pooling
      print
  TG.forkIO group $
    catchAll
      do
        server "7979" $ serverHandlerEdit pooling
      print
  TG.wait group

gadtMain :: IO ()
gadtMain = mainWith
  do
    \Pooling {..} -> gadtServerHandler parseQuery $
      hoistRespond runPooling responderQuery
  do
    \Pooling {..} -> gadtServerHandler parseEdit $
      hoistRespond runPooling responderEdit

existentailMain :: IO ()
existentailMain = mainWith 
  do \Pooling {..} -> existentialServerHandler $ hoistController runPooling queryController 
  do \Pooling {..} -> existentialServerHandler $ hoistController runPooling editController 