module UnixDomainSocket where

import Lib

import Control.Concurrent
import Data.List
import Network.Socket
import qualified Network.Socket.ByteString as NSBS
import Control.Monad
import System.Directory
import qualified Control.Exception as E
import qualified Data.Vector as V

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
