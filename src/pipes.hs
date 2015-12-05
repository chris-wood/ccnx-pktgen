-- networking stuff (laying the groundwork for sending right to certain sockets)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

sendToPipeUDP :: String -- host
           -> String -- port
           -> [ByteString]
           -> IO ()
sendToPipeUDP host port (bs:xs) = do
    (serveraddr:_) <- getAddrInfo Nothing (Just host) (Just port)
    s <- socket (addrFamily serveraddr) Datagram defaultProtocol
    connect s (addrAddress serveraddr)

    print bs
    send s bs
    sClose s

    sendToPipeUDP host port xs -- recurse. this is awful.
sendToPipeUDP host port [] = return()

sendToPipeTCP :: String
                -> String
                -> [ByteString]
                -> IO ()
sendToPipeTCP host port (bs:xs) = do
    (serverAddr:_) <- getAddrInfo Nothing (Just host) (Just port)
    s <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect s (addrAddress serverAddr)
    send s bs
    sClose s

    sendToPipeTCP host port xs
sendToPipeTCP host port [] = return()

-- sendToPipe "127.0.0.1" "9002" [(Data.ByteString.Char8.pack "Hello, World!")]
