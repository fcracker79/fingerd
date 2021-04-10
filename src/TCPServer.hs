{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module TCPServer where

import Control.Monad (forever, void)
import Control.Monad.Managed
  ( Managed
  , MonadIO (liftIO)
  , runManaged
  )
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
import Service

type HandleReq v = RespondG v Managed -> Socket -> Managed ()

handleUser :: HandleReq ViaUserK
handleUser (RespondG respond) soc = void $ do
  msg <- liftIO $ recv soc 1024
  liftIO . send soc =<< case msg of
    "\r\n" -> serialize <$> respond GetUsersReq
    name -> serialize <$> respond (GetUserReq $ decodeUtf8 name)

handleManagement :: HandleReq ViaManagementK
handleManagement (RespondG respond) soc = void $ do
  msg <- liftIO $ recv soc 1024
  let cmd = BS.head msg
  liftIO . send soc =<< case w2c cmd of
    '+' -> "OK" <$ respond (SaveUserReq user)
    '-' -> "NI" <$ respond (DeleteUserReq $ decodeUtf8 $ BS.tail msg)
    '~' -> "NI" <$ respond (UpdateUserReq user)
  where
    user :: User
    -- TODO user from string
    user = undefined

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

handleQueries :: (Socket -> Managed ()) -> Socket-> Managed ()
handleQueries handler sock = forever $ do
  (soc, _) <- liftIO $ accept sock
  liftIO $ putStrLn "got connection, handling query"
  handler soc
  liftIO $ close soc
