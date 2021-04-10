{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Server where

import Control.Monad (forever, void)
import Control.Monad.Managed
  ( Managed
  , MonadIO (liftIO)
  , runManaged
  )
import Controller
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Domain.User
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

serverHandler
  :: Parser v -- ^ a parser for the 'v' service
  -> Respond v Managed -- ^ handler for 'v' domain requests
  -> ServerHandler
serverHandler parse respond soc = void $ do
  msg <- liftIO $ recv soc 1024
  case parse msg of
    Nothing -> liftIO . send soc $ "not parse"
    Just input -> do
      Response output <- respond `applyRespond` input
      liftIO . send soc . render $ output

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
