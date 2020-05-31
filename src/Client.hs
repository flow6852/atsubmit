{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Client where

import Lib
import Types

import Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import qualified Data.Aeson as DA
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import Text.HTML.DOM
import Text.XML.Cursor
import Control.Monad
import Control.Applicative
import Control.Exception
import qualified Control.Foldl as CF
import qualified Control.Exception as E
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import System.Directory
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.Socket
import Network.HTTP.Types.Header

sendServer :: FilePath -> (Socket -> IO a) -> IO a
sendServer path client = withSocketsDo $ E.bracket (open path) close (\s -> client s `catch` \(e :: SHelperException) -> throwIO e)
 where
  open :: FilePath -> IO Socket
  open path = do
   s <- socket AF_UNIX Stream 0
   connect s (SockAddrUnix path)
   return s

evalSHelper :: SHelper a -> Socket -> IO a
evalSHelper (Login user pass) sock = do
  sendMsg sock ((toStrict.DA.encode) (LoginReq user pass)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (LoginRes unit)) -> return unit
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (QGet qname udir) sock = do
  sendMsg sock ((toStrict.DA.encode) (QGetReq qname udir)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (QGetRes qn)) -> return qn
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (CGet cname udir) sock = do
  Prelude.print ((toStrict.DA.encode) (CGetReq cname udir))
  sendMsg sock ((toStrict.DA.encode) (CGetReq cname udir)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  Prelude.print raw
  case DA.decode raw of
   Just (SHelperOk (CGetRes cn)) -> return cn
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (Test _ source qname) sock = do
  sendMsg sock ((toStrict.DA.encode) (TestReq source qname)) 1024
  testRsvloop sock 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (TestRes unit)) -> return unit
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (Submit source qname) sock = do
  sendMsg sock ((toStrict.DA.encode) (SubmitReq source qname)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (SubmitRes unit)) -> return unit
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (Types.Debug source din) sock = do
  Prelude.print ((toStrict.DA.encode) (DebugReq source din))
  sendMsg sock ((toStrict.DA.encode) (DebugReq source din)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  Prelude.print raw
  case DA.decode raw of
   Just (SHelperOk (DebugRes dbody)) -> return dbody
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper Print sock = do
  Prelude.print ((toStrict.DA.encode) PrintReq)
  sendMsg sock ((toStrict.DA.encode) PrintReq) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  Prelude.print raw
  case DA.decode raw of
   Just (SHelperOk (PrintRes pr)) -> return pr
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (Show qname) sock = do
  Prelude.print ((toStrict.DA.encode) (ShowReq qname))
  sendMsg sock ((toStrict.DA.encode) (ShowReq qname)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  Prelude.print raw
  case DA.decode raw of
   Just (SHelperOk (ShowRes qio)) -> return qio
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (Result cname) sock = do
  sendMsg sock ((toStrict.DA.encode) (ResultReq cname)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (ResultRes cr)) -> return cr
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper Stop sock = do
  sendMsg sock ((toStrict.DA.encode) StopReq) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (StopRes unit)) -> return unit
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper Logout sock = do
  sendMsg sock ((toStrict.DA.encode) LogoutReq) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (LogoutRes unit)) -> return unit
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

testRsvloop :: Socket -> Int -> IO (Maybe SHelperServerResponce)
testRsvloop sock size = do
 raw <- fromStrict <$> recvMsg sock size
 case DA.decode raw of
  Just AC -> Prelude.print "AC" >> testRsvloop sock size
  Just (WA out ans) -> Prelude.print out >> Prelude.print ans >> testRsvloop sock size
  Just (CE msg) -> Prelude.print msg >> testRsvloop sock size
  Just RE -> Prelude.print "RE" >> testRsvloop sock size
  Just TLE -> Prelude.print "TLE" >> testRsvloop sock size
  Just IE  -> Prelude.print "IE" >> throwIO InternalError
  _ -> case DA.decode raw of Just (SHelperOk (TestRes res)) -> return $ Just $ SHelperOk $ TestRes res
                             _ -> throwIO JsonParseError
