module Main where

import Lib
import Types
import Server
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
import Control.Concurrent
import System.FilePath

main :: IO ()
main = do
 TIO.putStrLn $ T.pack "start test."
 path <- getEnv "HOME"
 file_exists <- doesFileExist (path System.FilePath.</> cookieFile)
 dat <- (if file_exists then BSC.readFile (path System.FilePath.</> cookieFile) >>= (\x -> createContest V.empty (cookieCsrfToken x))
                        else createContest V.empty T.empty)
 contest <- newMVar dat
 let action = server (actionSHelper contest)
 runServer (path System.FilePath.</> sockpath) action
