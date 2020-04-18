{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Server.Debug where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Aeson
import Data.ByteString.Lazy
import Network.Socket
import System.Directory

atDebug :: AtFunc
atDebug contests msg = doesFileExist ((T.unpack.debug_input) msg) >>= \x -> if not x then return err else case file msg of
 Just fm -> do
  lang <- languageSelect (homedir contests) fm
  copyFile (T.unpack fm) ((T.unpack.main_file) contests)
  copyFile ((T.unpack.debug_input) msg) ((T.unpack.input_file) contests) 
  ec <- if is_docker lang then useDockerTest (docker_image lang) (T.append "Main." ((V.head.extention) lang)) contests
                          else unUseDocker (compile lang) (exec lang) (T.append "Main." ((V.head.extention) lang)) contests
  outres <- TIO.readFile ((T.unpack.output_file) contests)
  comp <- TIO.readFile ((T.unpack.compile_file) contests) 
  dinp <- TIO.readFile ((T.unpack.input_file) contests)
  return $ Right (case ec of
                       Just 0  -> (contests, createResAtSubmit 200 "debug accept" [[dinp , outres]])
                       Just 1  -> (contests, createResAtSubmit 200 "debug accept" [[dinp , comp]])
                       Just 2  -> (contests, createResAtSubmit 200 "debug accept" [[dinp , "runtime error"]])
                       Just _  -> (contests, createResAtSubmit 200 "debug accept" [[dinp , "time limit error"]])
                       Nothing -> (contests, createResAtSubmit 200 "debug error" [[dinp , "other error"]]))
 _  -> do
  return $ Left (400, "set question name and file name") -- nothing question
 where
  err = Left (400, T.append "doesn't file exists : " (debug_input msg))
