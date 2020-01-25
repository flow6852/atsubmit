module Main where

import Lib
import UnixDomainSocket
import AtCoderLib

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Environment
import System.IO

sockpath = "/.local/lib/atsubmit/atsubmit.sock"

main :: IO ()
main = do
 arg <- Prelude.map T.pack <$> getArgs
 if null arg then atLogin nullContest V.empty >>= \(b,c) -> if c == nullContest then putStrLn "authentication error..." 
                                                            else getEnv "HOME" >>= \path -> runServer c (path ++ sockpath) server 
 else getEnv "HOME">>= \path -> sendServer (path ++ sockpath) $ client arg
