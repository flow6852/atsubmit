{-# LANGUAGE OverloadedStrings #-}
module Server.Stop where

import Lib

import qualified Data.ByteString.Char8 as BSC
import Data.Text

atStop :: AtFunc
atStop contests msg = do
 BSC.writeFile (((unpack.homedir) contests) ++ cookieFile) ((BSC.unlines.cookie) contests)
 return (Right (contests, createResAtStatus 200 "server stopped."))
