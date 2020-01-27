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
  !(b,c) <- atLogin nullContest nullAtSubmit -- login 
  if Prelude.null (cookie c) then putStrLn "authentication error..." >> TIO.putStrLn (csrf_token c)
  else putStrLn "let's start!" >> getEnv "HOME" >>= \path -> daemonize $ runServer c (path ++ sockpath) server 
 else getEnv "HOME">>= \path -> sendServer (path ++ sockpath) $ client arg
