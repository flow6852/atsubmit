{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Client
import Types

import qualified Data.Text as T
import System.Environment
import System.Directory

sockpath = "/.local/lib/atsubmit/atsubmit.sock"

main :: IO ()
main = do
 path <- (\x -> x ++ sockpath) <$> getEnv "HOME"
 wd <- getCurrentDirectory
 arg <- Prelude.map T.pack <$> getArgs
 case arg of 
  ["login"] -> do
   [user, pass] <- map T.pack <$> getAtKeys 
   login path user pass
   Prelude.print "login accept."
  ["qget", qn] -> do
   res <- qget path qn wd
   Prelude.print res
  ["cget", cn] -> do
   res <- cget path cn wd
   Prelude.print res
  ["test", fn, qn] -> do
   res <- test path (T.unpack fn) qn wd
   Prelude.print "test accept."
  ["show", qn] -> do
   res <- Client.show path qn
   Prelude.print res
  ["show"] -> do 
   res <- Client.print path
   Prelude.print res
  ["debug", src, din] -> do
   res <- debug path (T.unpack src) (T.unpack din) wd
   Prelude.print res
  ["result", cn] -> do
   result <- result path cn
   Prelude.print result
  ["stop"] -> do
   stop path
   Prelude.print "server stopped."
  ["logout"] -> do
   logout path
   Prelude.print "logout accept."
