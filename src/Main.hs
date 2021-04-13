{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AccessControl
import Control.Concurrent.Async (waitBoth, withAsync)
import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.Catch (catchAll)
import Control.Monad.Managed (Managed, liftIO, managed, runManaged)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader
  ( MonadIO (liftIO)
  , ReaderT (runReaderT)
  , fix
  , void
  )
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Data (Data, Typeable)
import Data.Maybe (fromMaybe)
import Database
  ( createDatabase
  )
import Database.SQLite.Simple (Connection, withConnection)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, send)
import Service.Controller (Controller, editController, hoistController, queryController, runController)
import System.Console.CmdArgs (cmdArgs, def, help, opt, summary, (&=))
import TCPServer (server)
import Text.Printf (printf)

data Config = Config
  { queryPort :: String
  , editPort :: String
  , databasePath :: FilePath
  }
  deriving (Show, Data, Typeable)

config :: Config
config =
  Config
    { queryPort = "79" &= help "Standard fingerd port"
    , editPort = "7978" &= help "Edit records port"
    , databasePath = "finger.db" &= help "SQlite database path"
    }
    &= summary "Fingerd fake server"

main :: IO ()
main = catchAll
  do
    Config {..} <- cmdArgs config
    runManaged do
      connection <- managed (withConnection databasePath) >>= fairSingleThreading
      runAccessControl connection createDatabase
      queryA <-
        managed $
          withAsync $
            server queryPort $
              runManaged . serve (hoistController (runAccessControl connection) queryController)
      editA <-
        managed $
          withAsync $
            server editPort $
              runManaged . serve (hoistController (runAccessControl connection) editController)
      liftIO $
        printf
          "Fingerd up: derving on ports %s + %s, database open at \"%s\"\n"
          queryPort
          editPort
          databasePath
      liftIO $ void $ waitBoth queryA editA
  do
    \e -> do
      print e
      putStrLn "bye"

serve
  :: Controller Managed
  -> Socket
  -> Managed ()
serve controller soc = fix \loop -> do
  liftIO $ send soc "> "
  msg <- liftIO $ recv soc 1024
  case msg of
    "exit\r\n" -> pure ()
    _ -> do
      moutput <- runMaybeT $ runController controller msg
      liftIO . send soc . (<> "\r\n") $ fromMaybe "command not parsed" moutput
      loop
