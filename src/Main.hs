module Main where

import TCPServer (server)
import Domain.UserService (responder, ensureDatabase)
import Repository.Database (newPool)
import Control.Monad.Managed ( runManaged )

main :: IO ()
main = do 
  pool <- newPool "finger.db"
  runManaged $ ensureDatabase pool
  server "7979" $ responder pool
