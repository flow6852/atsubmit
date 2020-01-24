module Main where

import Lib
import UnixDomainSocket
import AtCoderLib

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Environment
import System.IO

sockpath = "socketest.sock"

main :: IO ()
main = do
 arg <- Prelude.map T.pack <$> getArgs
 if null arg then atLogin nullContest V.empty >>= \(b,c) -> runServer c sockpath server 
 else sendServer sockpath $ client arg

