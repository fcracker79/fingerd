{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module TCPServer where

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

handleQuery :: Parser v -> RespondG v Managed -> Socket -> Managed ()
handleQuery parse respond soc = void $ do
  msg <- liftIO $ recv soc 1024
  case parse msg of
    Nothing -> liftIO . send soc $ "not parse"
    Just input -> do
      Response output <- applyRespond respond input
      liftIO . send soc . render $ output

server :: ServiceName -> (Socket -> Managed ()) -> IO ()
server port handler = withSocketsDo $ do
  serveraddr : _ <- getAddrInfo
    do Just $ defaultHints {addrFlags = [AI_PASSIVE]}
    do Nothing
    do Just port
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  runManaged $ handleQueries handler sock
  close sock

handleQueries :: (Socket -> Managed ()) -> Socket -> Managed ()
handleQueries handler sock = forever $ do
  (soc, _) <- liftIO $ accept sock
  liftIO $ putStrLn "got connection, handling query"
  handler soc
  liftIO $ close soc
