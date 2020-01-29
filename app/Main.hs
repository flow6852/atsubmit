{-# LANGUAGE BangPatterns #-}
module Main where

import Lib
import UnixDomainSocket
import AtCoderLib

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
 if null arg then do
  (b,c) <- atLogin nullContest nullAtSubmit -- login 
  getEnv "HOME" >>= \path -> daemonize $ runServer c (path ++ sockpath) server
--  if Prelude.null (cookie c) then (TIO.putStr.T.unlines) b >> TIO.putStrLn (csrf_token c)
--  else (TIO.putStr.T.unlines) b >> getEnv "HOME" >>= \path -> daemonize $ runServer c (path ++ sockpath) server 
 else getEnv "HOME">>= \path -> sendServer (path ++ sockpath) $ client arg
