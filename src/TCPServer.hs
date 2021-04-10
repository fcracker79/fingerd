{-# LANGUAGE BlockArguments #-}

module TCPServer where

import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrFlags)
  , AddrInfoFlag (AI_PASSIVE)
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
  , withSocketsDo, ServiceName
  )
import Network.Socket.ByteString (recv, send, sendAll)
import Repository.UserRepository
import Service
import Control.Monad.Managed

{- logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  (soc, _) <- accept sock
  printAndKickback soc
  close soc
  where
    printAndKickback conn = do
      msg <- recv conn 1024
      print msg
      sendAll conn msg -}

server :: ServiceName -> Respond Managed -> IO ()
server port respond = withSocketsDo $ do
  serveraddr : _ <- getAddrInfo
    do Just $ defaultHints {addrFlags = [AI_PASSIVE]}
    do Nothing
    do Just port
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  runManaged $ handleQueries respond sock
  close sock

handleQuery :: Respond Managed -> Socket -> Managed ()
handleQuery respond soc = void $ do
  msg <- liftIO $ recv soc 1024
  liftIO . send soc =<< case msg of
    "\r\n" -> respond ReqUsers
    name -> respond $ ReqUser $ decodeUtf8 name

handleQueries :: Respond Managed -> Socket -> Managed ()
handleQueries respond sock = forever $ do
  (soc, _) <- liftIO $ accept sock  
  liftIO $ putStrLn "got connection, handling query"
  handleQuery respond soc
  liftIO $ close soc
