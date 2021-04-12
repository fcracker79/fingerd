{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Concurrent.Async (waitBoth, withAsync)
import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.Managed (Managed, liftIO, managed, runManaged)
import Data.Data (Data, Typeable)
import Domain.UserService (ensureDatabase)
import Lib.TCPServer (server)
import Network.Socket (Socket)
import Repository.Database (Pooling (Pooling), newPool, runPooling)
import Solution.Existential.Controller (editController, hoistController, queryController)
import Solution.Existential.Server (existentialServerHandler)
import Solution.GADTs.Controller (hoistRespond, parseEdit, parseQuery, responderEdit, responderQuery)
import Solution.GADTs.Server (gadtServerHandler)
import System.Console.CmdArgs (cmdArgs, def, help, opt, summary, (&=))
import Text.Printf (printf)
import Control.Monad.Catch (catch, catchAll)

data Config = Config {queryPort :: String, editPort :: String, databasePath :: FilePath} deriving (Show, Data, Typeable)

config :: Config
config =
  Config
    { queryPort = "79" &= help "Standard fingerd port"
    , editPort = "7978" &= help "Edit records port"
    , databasePath = "finger.db" &= help "SQlite database path" 
    }
    &= summary "Fingerd fake server"

main :: IO ()
main = do
  config <- cmdArgs config
  mainWith config gadtsServices

-- | use a socket to serve access to a DB connection
type Service = Pooling Managed -> Socket -> Managed ()

data Services = Services
  { -- | standard fingerd queries
    queryService :: Service
  , -- | record modification queries
    editService :: Service
  }

mainWith
  :: Config
  -> Services
  -> IO ()
mainWith Config {..} Services {..} = catchAll
  do
    runManaged do
      pooling <- newPool databasePath
      runPooling pooling ensureDatabase
      queryA <- managed $ withAsync
        do server queryPort $ runManaged . queryService pooling
      editA <- managed $ withAsync
        do server editPort $ runManaged . editService pooling
      liftIO $ printf "Fingerd up: derving on ports %s + %s, database open at \"%s\"\n" queryPort editPort databasePath
      liftIO $ void $ waitBoth queryA editA
  do \e -> do 
      print e 
      putStrLn "bye"

gadtsServices :: Services
gadtsServices = Services
  do
    \Pooling {..} ->
      gadtServerHandler parseQuery $
        hoistRespond runPooling responderQuery
  do
    \Pooling {..} ->
      gadtServerHandler parseEdit $
        hoistRespond runPooling responderEdit

-- untested
existentialServices :: Services
existentialServices = Services
  do \Pooling {..} -> existentialServerHandler $ hoistController runPooling queryController
  do \Pooling {..} -> existentialServerHandler $ hoistController runPooling editController
