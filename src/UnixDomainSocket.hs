{-# LANGUAGE OverloadedStrings #-}
module UnixDomainSocket where

import Lib

import Control.Concurrent
import Data.List
import Network.Socket
import qualified Network.Socket.ByteString as NSBS
import qualified Data.ByteString as S
import Data.ByteString.Lazy
import Control.Monad
import System.Directory
import qualified Control.Exception as E
import qualified Data.Vector as V
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Aeson as DA

data  Sizes = Sizes { socksize :: Int
                    , datasize :: Int
                    } deriving (Show, Eq)

instance DA.ToJSON Sizes where
 toJSON (Sizes s d) = DA.object [ "socksize" DA..= s, "datasize" DA..= d]

instance DA.FromJSON Sizes where
 parseJSON (DA.Object v) = Sizes <$> (v DA..: "socksize") <*> (v DA..: "datasize" )

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
 json <- fromStrict <$> NSBS.recv sock n
 case DA.decode json of
  Just size -> do
    NSBS.send sock $ toStrict.DA.encode $ size {socksize = min (socksize size) n} -- deside receive size
    rcv <- recvLoop sock (min (socksize size) n) 0
    return $ (Prelude.foldl1 S.append) rcv where
                                            recvLoop :: Socket -> Int -> Int -> IO [S.ByteString]
                                            recvLoop s k i = do
                                             msg <- NSBS.recv s k
                                             if (S.length msg) + i == (datasize size) then return [msg]
                                             else if (S.length msg) + i > (datasize size) then return [msg]
                                             else  recvLoop s k ((S.length msg) + i) >>= \next -> return (msg:next)
  _         -> return S.empty
 
sendMsg :: Socket -> S.ByteString -> Int -> IO ()
sendMsg sock msg n = do
 NSBS.send sock $ toStrict.DA.encode $ Sizes {socksize = n, datasize = S.length msg}
 json <- fromStrict <$> NSBS.recv sock n -- deside send size
 case DA.decode json of
  Just size -> sendLoop sock (takeNList (socksize size) msg)
  _         -> return ()
 where
  sendLoop :: Socket -> [S.ByteString] -> IO()
  sendLoop s m = do 
   NSBS.send s $ Prelude.head m
   if Prelude.length m == 1 then return () else  sendLoop s (Prelude.tail m)
