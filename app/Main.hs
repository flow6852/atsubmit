module Main where

import Lib

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO

main :: IO ()
main = do
 arg <- getArgs
 if null arg then startServer else sendServer arg

startServer :: IO ()
startServer = do
 Prelude.putStr "command $ "
 hFlush stdout
 command <- words <$> getLine
 case head command of
  "login"  -> putStrLn "atCoderLogin"
  "get"    -> putStrLn "getLatestPage"
  "show"   -> putStrLn "showLatestPage"
  "submit" -> putStrLn "submitLatest"
 startServer

sendServer :: [String] -> IO()
sendServer arg = putStrLn "inpuot"
