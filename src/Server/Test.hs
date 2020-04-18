{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Server.Test where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Aeson
import Data.ByteString.Lazy
import Network.Socket
import System.Directory

atTest :: Socket -> AtFunc
atTest sock contests msg = case (cname msg, qname msg, file msg) of
 (Just cm, Just qm, Just fm) -> do
  lang <- languageSelect (homedir contests) fm
  let func = if is_docker lang then useDockerTest (docker_image lang) 
                               else unUseDocker (compile lang) (exec lang)
  copyFile (T.unpack fm) ((T.unpack.main_file) contests)
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  case mquest of Nothing -> return $ Left (405, "not test case of questions") -- not getting
                 Just a  -> testLoop (qio a) func (T.append "Main." ((V.head.extention) lang)) 1 >>= 
                            \x -> return $ Right (contests, createResAtStatus (fst x) (snd x))
  where
   testLoop :: V.Vector (T.Text, T.Text) -> (T.Text -> Contest -> IO (Maybe Int)) -> T.Text -> Int -> IO (Int, T.Text)
   testLoop qs func main k = if V.null qs then return (200, "accepted test") else do
    let rmsg = if (V.null.V.tail) qs then "last" else "next"
    TIO.writeFile ((T.unpack.input_file) contests) $ (fst.V.head) qs
    ec <- func main contests
    outres <- TIO.readFile ((T.unpack.output_file) contests)
    comp <- TIO.readFile ((T.unpack.compile_file) contests) 
    let (out, rm) = case ec of
                     Just 0  -> if checkResult (T.lines outres) ((T.lines.snd.V.head) qs)
                              then ([[(T.pack.show) k, "AC"]], rmsg)
                              else ([[(T.pack.show) k, "WA", outres, (snd.V.head) qs]], rmsg)
                     Just 1  -> ([[(T.pack.show) k, "CE", comp]], "last")
                     Just 2  -> ([[(T.pack.show) k, "RE"]], rmsg)
                     Just _  -> ([[(T.pack.show) k, "TLE"]], rmsg)
                     Nothing -> ([[(T.pack.show) k, "Nothing"]], "last")
    sendMsg sock ((toStrict.encode.createResAtSubmit 200 rm) out) 1024
    if "last" /= rm then testLoop (V.tail qs) func main (k+1) else return (200, "accepted test")
 _  -> do
  sendMsg sock ((toStrict.encode.createResAtStatus 400) "last") 1024
  return $ Left (400, "set question name and file name") -- nothing question
