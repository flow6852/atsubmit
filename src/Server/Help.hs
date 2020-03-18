{-# LANGUAGE OverloadedStrings #-}

module Server.Help where

import Lib

import Data.Text
import qualified Data.Text.IO as TIO
import System.Directory

atHelp :: AtFunc
atHelp contests msg = getHomeDirectory >>= \dir -> TIO.readFile (dir ++ helpFile) >>=
                                            \x -> return $ Right (contests, createResAtSubmit 200 "help message" [[x]])

helpText :: IO Text
helpText = TIO.readFile helpFile
