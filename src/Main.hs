{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.Async (waitBoth, withAsync)
import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.Managed (Managed, liftIO, managed, runManaged)
import Domain.UserService (ensureDatabase)
import Lib.TCPServer (server)
import Network.Socket (Socket)
import Repository.Database (Pooling (Pooling), newPool, runPooling)
import Solution.Existential.Controller (editController, hoistController, queryController)
import Solution.Existential.Server (existentialServerHandler)
import Solution.GADTs.Controller (hoistRespond, parseEdit, parseQuery, responderEdit, responderQuery)
import Solution.GADTs.Server (gadtServerHandler)

main :: IO ()
main = gadtMain

type Handler = Pooling Managed -> Socket -> Managed ()

mainWith
  :: Handler
  -> Handler
  -> IO ()
mainWith serverHandlerQuery serverHandlerEdit = finally
  do
    runManaged do
      pooling <- newPool "finger.db"
      runPooling pooling ensureDatabase
      queryA <- managed $ withAsync
        do server "79" $ runManaged . serverHandlerQuery pooling
      editA <- managed $ withAsync
        do server "7979" $ runManaged . serverHandlerEdit pooling
      liftIO $ void $ waitBoth queryA editA
  do putStrLn "\nbye"

gadtMain :: IO ()
gadtMain = mainWith
  do
    \Pooling {..} ->
      gadtServerHandler parseQuery $
        hoistRespond runPooling responderQuery
  do
    \Pooling {..} ->
      gadtServerHandler parseEdit $
        hoistRespond runPooling responderEdit

-- untested
existentailMain :: IO ()
existentailMain = mainWith
  do \Pooling {..} -> existentialServerHandler $ hoistController runPooling queryController
  do \Pooling {..} -> existentialServerHandler $ hoistController runPooling editController
