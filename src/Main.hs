{-# LANGUAGE BlockArguments #-}
module Main where

import qualified Control.Concurrent.Thread.Group as TG
import Control.Monad.Managed (runManaged)
import Controller ( parseQuery, parseEdit )
import Domain.UserService (ensureDatabase, responderEdit, responderQuery)
import Repository.Database (newPool)
import Server ( serverHandler, server )
import Control.Monad.Catch

main :: IO ()
main = do
  pool <- newPool "finger.db"
  runManaged $ ensureDatabase pool
  group <- TG.new
  TG.forkIO group $
    catchAll 
      do server "79" $ serverHandler parseQuery $ responderQuery pool
      print 
  TG.forkIO group $
    catchAll 
      do server "7979" $ serverHandler parseEdit $ responderEdit pool
      print
  TG.wait group
