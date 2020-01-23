module UnixDomainSocket where

import Lib

import Control.Concurrent
import Data.List
import Network.Socket
import qualified Network.Socket.ByteString as NSBS
import Control.Monad
import System.Directory
import qualified Control.Exception as E

-- server part
runServer :: UserData -> FilePath -> (Socket -> UserData -> IO Bool) -> IO() -- [TODO] refactoring?
runServer user path server = withSocketsDo $ do E.bracket (open path) close (loop user)
 where
  loop :: UserData -> Socket -> IO()
  loop user s = do
   (conn, peer) <- accept s
   endCheck <- server conn user
   close conn
   if endCheck then return () else loop user s
  open :: FilePath -> IO Socket
  open path = do
   sock <- socket AF_UNIX Stream 0 -- AF_UNIX is UDS? [TODO] study...
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
sendServer path client = withSocketsDo $ do E.bracket (open path) close client
 where
  open :: FilePath -> IO Socket
  open path = do
   s <- socket AF_UNIX Stream 0
   connect s (SockAddrUnix path)
   return s
