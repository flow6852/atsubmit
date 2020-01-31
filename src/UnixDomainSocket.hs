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
import qualified Data.Text as T

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

recvMsg :: Socket -> Int -> IO S.ByteString
recvMsg sock n = do
 mlth <- read.T.unpack.decodeUtf8 <$> NSBS.recv sock n
 NSBS.send sock $ encodeUtf8.T.pack.show.min mlth $ n
 recvLoop sock (min mlth n) >>= return.Prelude.foldl1 S.append
  where
   recvLoop :: Socket -> Int -> IO [S.ByteString]
   recvLoop s k = do
    msg <- NSBS.recv s k 
    case decodeUtf8 msg of
     "0" -> NSBS.send s "0" >> return [] -- end recieve
     _   -> NSBS.send s "1" >> recvLoop s k >>= \next -> return (msg:next)
 
sendMsg :: Socket -> S.ByteString -> Int -> IO ()
sendMsg sock msg n = do
 NSBS.send sock $ (encodeUtf8.T.pack.show) n
 mlth <- min n.read.T.unpack.decodeUtf8 <$> NSBS.recv sock n
 sendLoop sock (takeNList mlth msg) mlth
  where
   sendLoop :: Socket -> [S.ByteString] -> Int -> IO()
   sendLoop s m k = do
    NSBS.send s $ if Prelude.null m then encodeUtf8 "0" else Prelude.head m -- end send / send head
    res <- decodeUtf8 <$> NSBS.recv s k
    case res of "0" -> return ()
                _   -> sendLoop s (Prelude.tail m) k >> return ()
