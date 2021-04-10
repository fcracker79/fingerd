module Main where

import qualified Control.Concurrent.Thread.Group as TG
import Control.Monad.Managed (runManaged)
import Domain.UserService (ensureDatabase, responderEdit, responderUser)
import Repository.Database (newPool)
import TCPServer

main :: IO ()
main = do
  pool <- newPool "finger.db"
  runManaged $ ensureDatabase pool
  group <- TG.new
  TG.forkIO group $ server "79" $ handleUser $ responderUser pool
  TG.forkIO group $ server "7979" $ handleEdit $ responderEdit pool
  TG.wait group
