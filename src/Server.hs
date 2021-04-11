{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Server where

import Control.Monad (forever, void)
import Control.Monad.Managed
  ( Managed
  , MonadIO (liftIO)
  , managed
  , runManaged
  , using
  )
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import ExistentialController (Controller, runController)
import GadtController
  ( Request
  , Respond
  , Response (Response)
  , applyRespond
  , render
  )
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrFlags)
  , AddrInfoFlag (AI_PASSIVE)
  , ServiceName
  , Socket
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultHints
  , defaultProtocol
  , getAddrInfo
  , listen
  , socket
  , withSocketsDo
  )
import Network.Socket.ByteString (recv, send, sendAll)
import Control.Concurrent.Thread (forkIO)
import Control.Monad.Catch (catchAll)
import Prelude hiding (bracket)
import Control.Exception (bracket, finally)
import Control.Concurrent.Async (async, withAsync, link, wait)
import Control.Monad.Fix (fix)

type ServerHandler = Socket -> Managed ()

-- build from GADTs base solution
gadtServerHandler
  :: Parser (Request v) -- ^ a parser for the 'v' service
  -> Respond v Managed -- ^ handler for 'v' domain requests
  -> ServerHandler
gadtServerHandler parse respond soc = void $ do
  msg <- liftIO $ recv soc 1024
  case parseOnly parse msg of
    Left e -> liftIO . send soc $ "not parse: " <> encodeUtf8 (T.pack e)
    Right input -> do
      Response output <- respond `applyRespond` input
      liftIO . send soc . (<> "\r\n") . render $ output  

-- | build from ControllerSimple solution
existentialServerHandler
  :: Controller Managed
  -> ServerHandler
existentialServerHandler controller soc = void $ do
  msg <- liftIO $ recv soc 1024
  moutput <- runMaybeT $ runController controller msg
  liftIO . send soc . (<> "\r\n") $ fromMaybe "command not parsed" moutput

server :: ServiceName -> ServerHandler -> IO ()
server port handler = withSocketsDo $ do
  serveraddr : _ <- getAddrInfo
    do Just $ defaultHints {addrFlags = [AI_PASSIVE]}
    do Nothing
    do Just port
  bracket
      do
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        bind sock (addrAddress serveraddr)
        listen sock 1
        -- print ("listening", sock) 
        pure sock
      do \s -> do 
          -- print ("listen closing", s) 
          close s
      do accepter $ runManaged . handler

accepter :: (Socket -> IO ()) -> Socket -> IO ()
accepter handler sock = fix \loop -> do
  asock <- fst <$> accept sock
  -- print ("accepting", asock) 
  withAsync 
    do finally 
        do handler asock 
        do 
          -- print ("accept closing", asock) 
          close asock 
    do \fork -> loop >> wait fork 

