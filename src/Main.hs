module Main where

import qualified Control.Concurrent.Thread.Group as TG
import Control.Monad.Managed (runManaged)
import Controller ( parseQuery, parseEdit )
import Domain.UserService (ensureDatabase, responderEdit, responderQuery)
import Repository.Database (newPool)
import Server ( serverHandler, server )

main :: IO ()
main = do
  pool <- newPool "finger.db"
  runManaged $ ensureDatabase pool
  group <- TG.new
  TG.forkIO group $
    server "79" $
      serverHandler parseQuery $ responderQuery pool
  TG.forkIO group $
    server "7979" $
      serverHandler parseEdit $ responderEdit pool
  TG.wait group
