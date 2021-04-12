{-# LANGUAGE BlockArguments #-}

module Solution.Existential.Server where

import Control.Monad (void)
import Control.Monad.Managed (Managed, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Maybe (fromMaybe)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, send)
import Solution.Existential.Controller (Controller, runController)

-- | serve the controller through a socket 
existentialServerHandler
  :: Controller Managed
  -> Socket
  -> Managed ()
existentialServerHandler controller soc = void $ do
  msg <- liftIO $ recv soc 1024
  moutput <- runMaybeT $ runController controller msg
  liftIO . send soc . (<> "\r\n") $ fromMaybe "command not parsed" moutput
