module Main where

import Lib
import Server
import Types

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BSC
import System.Directory
import System.Environment
import System.IO
import System.Posix.Daemonize
import Control.Concurrent

sockpath = "/.local/lib/atsubmit/atsubmit.sock"

main :: IO ()
main = do
 path <- getEnv "HOME"
 file_exists <- doesFileExist (path ++ cookieFile)
 dat <- (if file_exists then BSC.readFile (path ++ cookieFile) >>= (\x -> createContest V.empty (BSC.lines x) (cookieCsrfToken x))
                        else return nullContest)
 contest <- newMVar dat
 let action = server (actionSHelper contest)
 daemonize $ runServer (path ++ sockpath) action

