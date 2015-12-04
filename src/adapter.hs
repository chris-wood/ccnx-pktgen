module Adapter (
    connectToMetis
    , connectToAthena
    , connectToCcnLite
) where

import System.IO
import Network.Socket 

-- TODO: implement these functions -- they should all return sockets

openSocket :: String -> String -> IO Network.Socket.Socket
openSocket host port = do
    (serverAddr:_) <- getAddrInfo Nothing (Just host) (Just port)
    s <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect s
    return s
