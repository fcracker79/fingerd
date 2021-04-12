{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module TCPServer where

import Control.Concurrent.Async
import Control.Exception (bracket, finally)
import Control.Monad.Fix (fix)
import Network.Socket
  ( AddrInfo (AddrInfo, addrAddress, addrFamily, addrFlags)
  , AddrInfoFlag (AI_PASSIVE)
  , ServiceName
  , Socket
  , SocketOption (ReuseAddr)
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultHints
  , defaultProtocol
  , getAddrInfo
  , listen
  , setSocketOption
  , socket
  , withSocketsDo
  )
import Network.Socket.ByteString (send)

type WithSocket = Socket -> IO ()

server :: ServiceName -> WithSocket -> IO ()
server port withSocket = withSocketsDo $ do
  AddrInfo {..} : _ <- getAddrInfo
    do Just $ defaultHints {addrFlags = [AI_PASSIVE]}
    do Nothing
    do Just port
  bracket
    do
      sock <- socket addrFamily Stream defaultProtocol
      setSocketOption sock ReuseAddr 1
      bind sock addrAddress
      listen sock 1
      pure sock
    do close
    do \sock -> fix \loop -> do
          asock <- fst <$> accept sock
          withAsync
            do
              finally
                do send asock "hello\n" >> withSocket asock
                do send asock "bye\n" >> close asock
            do \fork -> link fork >> loop
