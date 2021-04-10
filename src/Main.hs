module Main where

import TCPServer (server, serverForEdit)
import Domain.UserService (responderUser, ensureDatabase, responderManagement)
import Repository.Database (newPool)
import Control.Monad.Managed ( runManaged )
import GHC.Conc (forkIO,)
import qualified Control.Concurrent.Thread.Group as TG
main :: IO ()
main = do 
  pool <- newPool "finger.db"
  runManaged $ ensureDatabase pool
  group <- TG.new
  TG.forkIO group $ server "79" $ responderUser pool
  TG.forkIO group $ serverForEdit "7979" $ responderManagement pool
  TG.wait group
