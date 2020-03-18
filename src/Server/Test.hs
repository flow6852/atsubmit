{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Server.Test where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Turtle

atTest :: AtFunc
atTest contests msg = case (cname msg, qname msg, file msg) of
 (Just cm, Just qm, Just fm) -> do
  lang <- languageSelect (homedir contests) fm
  TIO.readFile (T.unpack (V.foldl1 T.append [userdir msg, "/", fm])) >>= TIO.writeFile (T.unpack mainfile)
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  case mquest of Nothing -> return $ Left (405, "not test case of questions") -- not getting
                 Just a  -> testLoop (qio a) (homedir contests) lang 1 >>= 
                            \x -> return $ Right (contests, createResAtSubmit 200 "accept test" x)
 _  -> return $ Left (400, "set question name and file name") -- nothing question
 where
  mainfile = T.append (homedir contests) "/.cache/atsubmit/src/source.txt"

testLoop :: V.Vector (T.Text, T.Text) -> T.Text -> LangJson -> Int -> IO [[T.Text]]
testLoop qs dir lang k = if V.null qs then return [] else do
 TIO.writeFile (T.unpack infile) $ (fst.V.head) qs
 TIO.writeFile (T.unpack outfile) $ (snd.V.head) qs
 ec <- shell (V.foldl1 T.append [dockershell, " ", docker_image lang, " ", main_file lang]) Turtle.empty
 outres <- TIO.readFile (T.unpack outfile)
 comp <- TIO.readFile (T.unpack compfile)
 (out, next) <- (
  \x -> case ec of
             ExitFailure 1 -> ([(T.pack.show) k, "CE", comp], [])
             ExitFailure 2 -> ([(T.pack.show) k, "RE"], x)
             ExitFailure _ -> ([(T.pack.show) k, "TLE"], x)
             ExitSuccess   -> (if checkResult (T.lines outres) ((T.lines.snd.V.head) qs) then ([(T.pack.show) k, "AC"], x)
                               else ([(T.pack.show) k, "WA", outres, (snd.V.head) qs], x))) <$> testLoop (V.tail qs) dir lang (k+1)
 return $ out:next
 where
  checkResult :: [T.Text] -> [T.Text] -> Bool
  checkResult [] []           = True
  checkResult ([]:es) (ans)   = checkResult es ans
  checkResult (r:es) (a:ns)   = if r == a then checkResult es ns else False
  checkResult _ _             = False
  dockershell = T.append dir "/.local/lib/atsubmit/docker_judge.sh"
  infile = T.append dir "/.cache/atsubmit/src/input.txt"
  outfile = T.append dir "/.cache/atsubmit/src/outres.txt"
  compfile = T.append dir "/.cache/atsubmit/src/comp.txt"

