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
  sendMsg sock ((toStrict.DA.encode) (CGetReq cname udir)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (CGetRes cn)) -> return cn
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (Test _ source qname) sock = do
  sendMsg sock ((toStrict.DA.encode) (TestReq source qname)) 1024
  raw <- testRsvloop sock 1024
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
  sendMsg sock ((toStrict.DA.encode) (DebugReq source din)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (DebugRes dbody)) -> return dbody
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper Print sock = do
  sendMsg sock ((toStrict.DA.encode) PrintReq) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (PrintRes pr)) -> return pr
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (Show qname) sock = do
  sendMsg sock ((toStrict.DA.encode) (ShowReq qname)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (ShowRes question)) -> return question
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (Result cname sid) sock = do
  sendMsg sock ((toStrict.DA.encode) (ResultReq cname sid)) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (ResultRes cr)) -> return cr
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper Log sock = do
  sendMsg sock ((toStrict.DA.encode) LogReq) 1024
  raw <- fromStrict <$> recvMsg sock 1024
  case DA.decode raw of
   Just (SHelperOk (LogRes ret)) -> return ret
   Just (SHelperErr e) -> throwIO e 
   _ -> throwIO Unknown

evalSHelper (LangId lang) sock = do
 sendMsg sock ((toStrict.DA.encode) (LangIdReq lang)) 1024
 raw <- fromStrict <$> recvMsg sock 1024
 case DA.decode raw of
  Just (SHelperOk (LangIdRes lid)) -> return lid
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

testRsvloop :: Socket -> Int -> IO ByteString
testRsvloop sock size = do
 raw <- fromStrict <$> recvMsg sock size
 case DA.decode raw of
  Just AC -> TIO.putStrLn "AC" >> testRsvloop sock size
  Just (WA out ans) -> waPrint out ans >> testRsvloop sock size
  Just (CE (Message msg)) -> TIO.putStrLn msg >> testRsvloop sock size
  Just RE -> TIO.putStrLn "RE" >> testRsvloop sock size
  Just TLE -> TIO.putStrLn "TLE" >> testRsvloop sock size
  Just IE  -> TIO.putStrLn "IE" >> throwIO InternalError
  _ -> return raw
  where
   waPrint :: TOut -> TAns -> IO()
   waPrint (TOut out) (TAns ans) = do
    TIO.putStrLn "====== output ======"
    TIO.putStrLn out
    TIO.putStrLn "====== answer ======"
    TIO.putStrLn ans
