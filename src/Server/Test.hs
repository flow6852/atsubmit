{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Server.Test where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Aeson
import Data.ByteString.Lazy
import Turtle
import Turtle.Line
import qualified Control.Foldl as CF
import Network.Socket
import System.Directory

atTest :: Socket -> AtFunc
atTest sock contests msg = case (cname msg, qname msg, file msg) of
 (Just cm, Just qm, Just fm) -> do
  lang <- languageSelect (homedir contests) fm
  let func = if is_docker lang then useDockerTest (docker_image lang) 
                               else unUseDocker (compile lang) (exec lang)
  copyFile (T.unpack sourcefile) (T.unpack mainfile)
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  case mquest of Nothing -> return $ Left (405, "not test case of questions") -- not getting
                 Just a  -> testLoop (qio a) func (main_file lang) 1 >>= 
                            \x -> return $ Right (contests, createResAtStatus (fst x) (snd x))
  where
   mainfile = T.append (homedir contests) "/.cache/atsubmit/src/source.txt"
   infile = T.append (homedir contests) "/.cache/atsubmit/src/input.txt"
   outfile = T.append (homedir contests) "/.cache/atsubmit/src/outres.txt"
   compfile = T.append (homedir contests) "/.cache/atsubmit/src/comp.txt"
   sourcefile = V.foldl1 T.append [(userdir msg) , "/", fm]
   testLoop :: V.Vector (T.Text, T.Text) -> (Maybe T.Text-> IO (Maybe Int)) -> (Maybe T.Text) -> Int -> IO (Int, T.Text)
   testLoop qs func main k = if V.null qs then return (200, "accepted test") else do
    let rmsg = if (V.null.V.tail) qs then "last" else "next"
    TIO.writeFile (T.unpack infile) $ (fst.V.head) qs
    ec <- func main
    outres <- TIO.readFile (T.unpack outfile)
    comp <- TIO.readFile (T.unpack compfile) 
    let (out, rm) = case ec of
                     Just 0  -> if checkResult (T.lines outres) ((T.lines.snd.V.head) qs)
                              then ([[(T.pack.show) k, "AC"]], rmsg)
                              else ([[(T.pack.show) k, "WA", outres, (snd.V.head) qs]], rmsg)
                     Just 1  -> ([[(T.pack.show) k, "CE", comp]], "last")
                     Just 2  -> ([[(T.pack.show) k, "RE"]], rmsg)
                     Just _  -> ([[(T.pack.show) k, "TLE"]], rmsg)
                     Nothing -> ([[(T.pack.show) k, "Nothing"]], rmsg)
    sendMsg sock ((toStrict.encode.createResAtSubmit 200 rm) out) 1024
    if "last" /= rm then testLoop (V.tail qs) func main (k+1) else return (200, "accepted test")
   checkResult :: [T.Text] -> [T.Text] -> Bool
   checkResult [] []           = True
   checkResult ([]:es) (ans)   = checkResult es ans
   checkResult (r:es) (a:ns)   = if r == a then checkResult es ns else False
   checkResult _ _             = False
   useDockerTest :: Maybe T.Text -> Maybe T.Text -> IO (Maybe Int)
   useDockerTest (Just image) (Just main) = do
    shell (V.foldl1 T.append ["docker create --name atsubmit_run --pids-limit 10 --network \"none\" ", image]) Turtle.empty
    shell (V.foldl1 T.append ["docker cp ", sourcefile, " atsubmit_run:/home/", main]) Turtle.empty
    shell (V.foldl1 T.append ["docker cp ", infile, " atsubmit_run:/home/input.txt"]) Turtle.empty
    shell "docker start atsubmit_run" Turtle.empty
    ec <- shell "timeout 2 docker wait atsubmit_run" Turtle.empty
    timecheck <- Turtle.fold (inshell "docker inspect atsubmit_run --format=\'{{.State.ExitCode}}\'" Turtle.empty) CF.head
    shell (V.foldl1 T.append ["docker cp atsubmit_run:/home/output.txt ", outfile]) Turtle.empty
    shell (V.foldl1 T.append ["docker cp atsubmit_run:/home/comp.txt ", compfile]) Turtle.empty
    shell "docker rm -f atsubmit_run" Turtle.empty
    case (ec,timecheck) of
     (ExitSuccess, Just "0") -> return $ Just 0
     (ExitFailure n, Just m) -> return $ Just $ if n == (read.T.unpack.lineToText) m then n else (read.T.unpack.lineToText) m 
   useDockerTest _ _ = return Nothing 
   unUseDocker :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Maybe Int)
   unUseDocker (Just compcmd) (Just execmd) (Just main) = do
    copyFile (T.unpack sourcefile) (T.unpack main)
    comp <- shell (V.foldl1 T.append [compcmd, " > ",compfile, " 2>&1"]) Turtle.empty
    case comp of
     ExitFailure _ -> return $ Just 1
     ExitSuccess -> do
      exe <- shell (V.foldl1 T.append ["timeout 2 sh -c \"", execmd, " < ", infile, " > ", outfile, "\""]) Turtle.empty
      case exe of
       ExitFailure 124 -> return $ Just 124
       ExitFailure n   -> return $ Just 2
       ExitSuccess     -> return $ Just 0
   unUseDocker _ _ _ = return Nothing
 _  -> return $ Left (400, "set question name and file name") -- nothing question
