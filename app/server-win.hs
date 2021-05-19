{-# LANGUAGE OverloadedStrings #-}
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
import System.FilePath
import Control.Concurrent

main :: IO ()
main = do
 path <- getEnv "HOME"
 arg <- Prelude.map T.pack <$> getArgs
 file_exists <- doesFileExist (path </> cookieFile)
 dat <- (if file_exists then BSC.readFile (path </> cookieFile) >>= (\x -> createContest V.empty (cookieCsrfToken x))
                        else createContest V.empty T.empty)
 contest <- newMVar dat
 let action = server (actionSHelper contest)
  runServer (path </> sockpath) action
