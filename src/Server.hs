{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Server where

import Control.Monad (forever, void)
import Control.Monad.Managed
  ( Managed
  , MonadIO (liftIO)
  , runManaged
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
      liftIO . send soc . render $ output

-- | build from ControllerSimple solution
existentialServerHandler
  :: Controller Managed
  -> ServerHandler
existentialServerHandler controller soc = void $ do
  msg <- liftIO $ recv soc 1024
  moutput <- runMaybeT $ runController controller msg
  liftIO . send soc $ fromMaybe "command not parsed" moutput

server :: ServiceName -> ServerHandler -> IO ()
server port handler = withSocketsDo $ do
  serveraddr : _ <- getAddrInfo
    do Just $ defaultHints {addrFlags = [AI_PASSIVE]}
    do Nothing
    do Just port
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  runManaged $ accepter handler sock
  close sock

accepter :: ServerHandler -> Socket -> Managed ()
accepter handler sock = forever $ do
  (soc, _) <- liftIO $ accept sock
  liftIO $ putStrLn "got connection, handling query"
  handler soc
  liftIO $ close soc
