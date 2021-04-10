module Main where

import qualified Control.Concurrent.Thread.Group as TG
import Control.Monad.Managed (runManaged)
import Domain.UserService (ensureDatabase, responderEdit, responderQuery)
import Repository.Database (newPool)
import TCPServer
import Controller

main :: IO ()
main = do
  pool <- newPool "finger.db"
  runManaged $ ensureDatabase pool
  group <- TG.new
  TG.forkIO group $ server "79" $ handleQuery parseQuery $ responderQuery pool
  TG.forkIO group $ server "7979" $ handleQuery parseEdit $ responderEdit pool
  TG.wait group
