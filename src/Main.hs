{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Control.Concurrent.Thread.Group as TG
import Control.Monad.Catch
import Control.Monad.Managed (runManaged)
import Control.Monad.Reader
import Controller (hoistRespond, parseEdit, parseQuery)
import Domain.UserService (ensureDatabase, responderEdit, responderQuery)
import Repository.Database (Pooling (Pooling), newPool)
import Server (server, serverHandler)

main :: IO ()
main = do
  Pooling pool <- newPool "finger.db"
  runManaged $ pool ensureDatabase
  group <- TG.new
  TG.forkIO group $
    catchAll
      do
        server "79" $
          serverHandler parseQuery $
            hoistRespond pool responderQuery
      print
  TG.forkIO group $
    catchAll
      do
        server "7979" $
          serverHandler parseEdit $
            hoistRespond pool responderEdit
      print
  TG.wait group
