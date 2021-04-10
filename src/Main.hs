module Main where

import qualified Control.Concurrent.Thread.Group as TG
import Control.Monad.Managed (runManaged)
import Domain.UserService (ensureDatabase, responderManagement, responderUser)
import GHC.Conc (forkIO)
import Repository.Database (newPool)
import TCPServer

main :: IO ()
main = do
  pool <- newPool "finger.db"
  runManaged $ ensureDatabase pool
  group <- TG.new
  TG.forkIO group $ server "79" $ handleUser $ responderUser pool 
  TG.forkIO group $ server "7979" $ handleManagement $ responderManagement pool
  TG.wait group
