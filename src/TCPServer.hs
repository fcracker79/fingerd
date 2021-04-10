module TCPServer where


import Control.Monad (forever)
import Network.Socket
    ( bind, close, defaultHints,
      getAddrInfo,
      withSocketsDo,
      accept,
      listen,
      socket,
      defaultProtocol,
      AddrInfo(addrFlags, addrFamily, addrAddress),
      AddrInfoFlag(AI_PASSIVE),
      Socket,
      SocketType(Stream) )
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad ( forever )

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do 
  (soc, _) <- accept sock
  printAndKickback soc
  close soc
  where printAndKickback conn = do
              msg <- recv conn 1024
              print msg
              sendAll conn msg


execute :: IO ()
execute = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  logAndEcho sock
  close sock


handleQuery :: Socket -> IO ()
handleQuery soc = do
    msg <- recv soc 1024
    case msg of 
        "\r\n" -> returnUsers soc
        name -> returnUser soc (decodeUtf8 name)


handleQueries :: Socket -> IO ()
handleQueries sock = forever $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    handleQuery soc
    close soc
