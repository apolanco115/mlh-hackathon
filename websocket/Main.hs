module Main where

import Network.Socket
import System.IO

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO()
mainLoop sock = do 
    conn <- accept sock
    runConn conn
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO()
runConn (sock, _) =  do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hello!"
    hClose hdl