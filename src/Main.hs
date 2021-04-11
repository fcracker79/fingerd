{-# LANGUAGE BlockArguments #-}

{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Catch
import Control.Monad.Managed (Managed, runManaged, managed)
import Control.Monad.Reader
import GadtController (hoistRespond, parseEdit, parseQuery, responderQuery, responderEdit)
import Repository.Database (Pooling (Pooling), WithPool, newPool, runPooling)
import Server (ServerHandler, gadtServerHandler, server, existentialServerHandler)
import Network.Socket (Socket)
import ExistentialController (queryController, hoistController, editController)
import Control.Monad.Morph (hoist)
import Domain.UserService (ensureDatabase)
import Control.Concurrent.Async (withAsync, waitBoth)


main :: IO ()
main = gadtMain 

mainWith
  :: (Pooling Managed -> ServerHandler) -- ^ 
  -> (Pooling Managed -> ServerHandler) -- ^ 
  -> IO ()
mainWith serverHandlerQuery serverHandlerEdit = do
  pooling <- newPool "finger.db"
  runManaged do   
    runPooling pooling ensureDatabase
    q <- managed $ withAsync 
        do server "79" $ serverHandlerQuery pooling
    m <- managed $ withAsync 
        do server "7979" $ serverHandlerEdit pooling
    liftIO $ void $ waitBoth q m 
  putStrLn "bye"

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