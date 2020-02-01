{-# LANGUAGE BangPatterns #-}
module Main where

import Lib
import UnixDomainSocket
import AtSubmitServer
import AtSubmitClient

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Environment
import System.IO
import System.Posix.Daemonize
import Control.Exception

sockpath = "/.local/lib/atsubmit/atsubmit.sock"

main :: IO ()
main = do
 arg <- Prelude.map T.pack <$> getArgs
 if null arg then do
  (c, b) <- atLogin nullContest nullReqAtSubmit `catch` (\e ->
            TIO.putStrLn ((T.pack.displayException) (e :: SomeException)) >> return (nullContest, nullResAtSubmit))  -- login 
  if (resstatus b) /= 200 then TIO.putStrLn $ resmsg b 
  else (TIO.putStrLn.resmsg) b >>  getEnv "HOME" >>= \path -> {-  daemonize $ -} runServer c (path ++ sockpath) server 
 else getEnv "HOME">>= \path -> sendServer (path ++ sockpath) $ client arg
