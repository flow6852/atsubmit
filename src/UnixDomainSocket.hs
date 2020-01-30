{-# LANGUAGE OverloadedStrings #-}
module UnixDomainSocket where

import Lib

import Control.Concurrent
import Data.List
import Network.Socket
import qualified Network.Socket.ByteString as NSBS
import qualified Data.ByteString as S
import Control.Monad
import System.Directory
import qualified Control.Exception as E
import qualified Data.Vector as V
import Data.Text.Encoding

-- server part
runServer :: Contest -> FilePath -> (Socket -> Contest -> IO (Bool, Contest)) -> IO()
runServer contest path server = withSocketsDo $ E.bracket (open path) close (loop contest)
 where
  loop :: Contest -> Socket -> IO()
  loop contest s = do
   (conn, peer) <- accept s
   (endCheck, next) <- server conn contest
   close conn
   if endCheck then return () else loop next s
  open :: FilePath -> IO Socket
  open path = do
   sock <- socket AF_UNIX Stream 0
   rmFile path
   ready sock
  ready s = do  
   bind s (SockAddrUnix path)
   listen s 1
   return s -- ready
  rmFile :: FilePath -> IO()
  rmFile path = doesFileExist path >>= \x -> when x (removeFile path)

-- client part
sendServer :: FilePath -> (Socket -> IO()) -> IO()
sendServer path client = withSocketsDo $ E.bracket (open path) close client
 where
  open :: FilePath -> IO Socket
  open path = do
   s <- socket AF_UNIX Stream 0
   connect s (SockAddrUnix path)
   return s

recvMsg :: Socket -> Int -> IO [S.ByteString] 
recvMsg sock n = do
 S.putStrLn "server recieve"
 msg <- NSBS.recv sock n
 print msg
 S.putStrLn "server send"
 if "." == decodeUtf8 msg then NSBS.send sock (encodeUtf8 "0") >> return []
                          else NSBS.send sock (encodeUtf8 "1") >> recvMsg sock n >>= \next -> return (msg:next)
 
sendMsg :: Socket -> [S.ByteString] -> Int -> IO ()
sendMsg sock msg n = do
 print msg
 S.putStrLn "send"
 NSBS.send sock $ if Prelude.null msg then "." else  Prelude.head msg
 S.putStrLn "recv"
 res <- decodeUtf8 <$> NSBS.recv sock n
 case res of "0" -> return ()
             "1" -> sendMsg sock (Prelude.tail msg) n
