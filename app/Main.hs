module Main where

import Lib
import Server
import Server.Login
import Client

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BSC
import System.Directory
import System.Environment
import System.IO
import System.Posix.Daemonize

sockpath = "/.local/lib/atsubmit/atsubmit.sock"

main :: IO ()
main = do
 path <- getEnv "HOME"
 file_exists <- doesFileExist (path ++ cookieFile)
 arg <- Prelude.map T.pack <$> getArgs
 if null arg then (if file_exists then BSC.readFile (path ++ cookieFile) >>= 
                     (\x -> createContest V.empty (BSC.lines x) (scrapingCsrfToken x))
                   else newLogin) >>= \c -> daemonize $ runServer c (path ++ sockpath) server
 else sendServer (path ++ sockpath) $ client arg

newLogin :: IO Contest
newLogin = do
 [user, pass] <- getAtKeys
 result <- atLogin nullContest (nullReqAtSubmit{username = Just (T.pack user), password = Just (T.pack pass)})
 case result of
  Left (c, b)  -> return nullContest
  Right (c, b) -> return c
