{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module Server where

import Lib
import Server.Login
import Server.GetPage
import Server.Submit
import Server.ShowPage
import Server.Result
import Server.Test
import Server.Logout
import Server.Help

import Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Aeson as DA
import qualified Data.ByteString as BS
import Control.Monad
import Control.Applicative
import Control.Exception
import qualified Control.Foldl as CF
import qualified Control.Exception as E
import System.Directory
import Network.Socket

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

server :: Socket -> Contest -> IO (Bool, Contest)
server sock contests = do
 json <- fromStrict <$> recvMsg sock 1024 
 case DA.decode json :: Maybe ReqAtSubmit of
  Nothing -> sendMsg sock (errMsg "server : json parse error") 1024 >> return (False, contests)
  Just x  -> do
   let (func, retb) =  case (T.unpack.subcmd) x of
                            "stop"   -> (atLogout, True)
                            "get"    -> (atGetPage, False)
                            "show"   -> (atShowPage, False)
                            "submit" -> (atSubmit, False)
                            "test"   -> (atTest sock, False)
                            "login"  -> (atLogin, False)
                            "result" -> (atResult, False)
                            "help"   -> (atHelp, False)
                            _        -> (notDo, False)
   (retc, res) <- (\result -> case result of Left  (ei, em) -> (contests, createResAtStatus ei (T.append "server error : " em))
                                             Right y        -> y) <$> func contests x
   print res
   sendMsg sock ((toStrict.DA.encode) res) 1024
   return (retb, retc)
 where
  notDo :: AtFunc
  notDo c m = return $ Left (400, "sub command undefined.")
  errMsg :: T.Text -> BS.ByteString
  errMsg = toStrict.DA.encode.createResAtStatus 405


