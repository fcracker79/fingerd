{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Solution.GADTs.Server where

import Control.Monad (void)
import Control.Monad.Managed (Managed, liftIO)
import Data.Attoparsec.ByteString (Parser, parseOnly)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, send)
import Solution.GADTs.Controller (Request (Request), Respond, Response (Response), applyRespond, render)

gadtServerHandler
  :: Parser (Request v) -- ^ a parser for the 'v' service
  -> Respond v Managed -- ^ handler for 'v' domain requests
  -> Socket
  -> Managed ()
gadtServerHandler parse respond soc = void $ do
  msg <- liftIO $ recv soc 1024
  case parseOnly parse msg of
    Left e -> liftIO . send soc $ "not parse: " <> encodeUtf8 (T.pack e)
    Right input -> do
      Response output <- respond `applyRespond` input
      liftIO . send soc . (<> "\r\n") . render $ output
