module Main where

import Lib
import Server
import Server.Login
import Client

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Environment
import System.IO
import System.Posix.Daemonize

sockpath = "/.local/lib/atsubmit/atsubmit.sock"

main :: IO ()
main = do
 arg <- Prelude.map T.pack <$> getArgs
 if null arg then getAtKeys >>= 
  \[user, pass] -> atLogin nullContest (nullReqAtSubmit{username = Just (T.pack user), password = Just (T.pack pass)}) >>= 
  \result -> case result of 
   Left (c, b)  -> TIO.putStrLn b 
   Right (c, b) -> (TIO.putStrLn.resmsg) b >> 
                   getEnv "HOME" >>= 
                   \path -> {- daemonize $ -} runServer c (path ++ sockpath) server
 else getEnv "HOME" >>= \path -> sendServer (path ++ sockpath) $ client arg
