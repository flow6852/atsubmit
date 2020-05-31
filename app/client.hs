{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Lib
import Client
import Types

import qualified Data.Text as T
import System.Environment
import System.Directory
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Control.Exception
import qualified Data.List as L

sockpath = "/.local/lib/atsubmit/atsubmit.sock"

main :: IO ()
main = do
 path <- (++ sockpath) <$> getEnv "HOME"
 wd <- getCurrentDirectory
 arg <- Prelude.map T.pack <$> getArgs
 case arg of 
  ["login"] -> login path `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["qget", qn] -> qget path qn wd `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["cget", cn] -> cget path cn wd `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["test", qn, fn] -> test path (T.unpack fn) qn wd `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["show", qn] -> Main.show path qn `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["print"] -> Main.print path `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["submit", qn, fn] -> submit path (T.unpack fn) qn wd `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["debug", src, din] -> debug path (T.unpack src) (T.unpack din) wd `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["result", cn] -> result path cn `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["stop"] -> stop path `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e
  ["logout"] -> logout path `catch` \(e :: SHelperException) -> TIO.putStrLn $ exceptionText e

login :: FilePath -> IO ()
login path = do
 [user, pass] <- map T.pack <$> getAtKeys 
 sendServer path (evalSHelper (Login (Username user) (Password pass)))
 TIO.putStrLn "login accept."

qget :: FilePath -> T.Text -> FilePath -> IO ()
qget path qn wd = do
 (QName res) <- sendServer path (evalSHelper (QGet (QName qn) (Userdir wd)))
 TIO.putStrLn res

cget :: FilePath -> T.Text -> FilePath -> IO ()
cget path cn wd = do
 res <- sendServer path (evalSHelper (CGet (CName cn) (Userdir wd)))
 Prelude.print res

test :: FilePath -> FilePath -> T.Text -> FilePath -> IO ()
test path fn qn wd = do
 sendServer path (\x ->  evalSHelper (Test x (Source (wd ++ ('/':fn))) (QName qn)) x)
 TIO.putStrLn "test accept."

show :: FilePath -> T.Text -> IO ()
show path qn = do
 (QIO res) <- sendServer path (evalSHelper (Show (QName qn)))
 showPrint $ V.toList res
  where
   showPrint :: [(T.Text, T.Text)] -> IO ()
   showPrint [] = return ()
   showPrint (x:xs) = do
    TIO.putStrLn "===== input ====="
    TIO.putStrLn $ fst x
    TIO.putStrLn "===== putput ====="
    TIO.putStrLn $ snd x
    showPrint xs

print :: FilePath -> IO ()
print path = do
 res <- sendServer path (evalSHelper Print)
 printFunc $ V.toList res
  where
   printFunc :: [QName] -> IO ()
   printFunc [] = return ()
   printFunc ((QName q):qs) = do
    TIO.putStrLn q
    printFunc qs

submit :: FilePath -> FilePath -> T.Text -> FilePath -> IO ()
submit path fn qn wd = do
 sendServer path (evalSHelper (Submit (Source (wd ++ ('/':fn))) (QName qn)))
 TIO.putStrLn "submit."

debug :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
debug path src din wd = do
 res <- sendServer path (evalSHelper (Types.Debug (Source (wd ++ ('/':src))) (DIn (wd ++ ('/':din)))))
 case res of
  DAC (DOut dout) -> TIO.putStrLn dout
  DCE (Message message) -> TIO.putStrLn message
  DRE -> TIO.putStrLn "runtime error."
  DTLE -> TIO.putStrLn "time limit error."
  DIE -> TIO.putStrLn "internal error."

result :: FilePath -> T.Text -> IO ()
result path cn = do
 result <- sendServer path (evalSHelper (Result (CName cn)))
 case result of CResult res -> mapM_ (TIO.putStrLn.T.intercalate " ") res

stop :: FilePath -> IO ()
stop path = do
 sendServer path (evalSHelper Stop)
 TIO.putStrLn "server stopped."

logout :: FilePath -> IO()
logout path = do
 sendServer path (evalSHelper Logout)
 TIO.putStrLn "logout accept."

exceptionText :: SHelperException -> T.Text
exceptionText e = case e of
 FailLogin -> "login failed."
 AlreadyGet -> "already get."
 NotExistsContest -> "not contest exist."
 QuestionNotFound -> "question not found."
 NotGetQuestion (QName a) -> T.append "not get : " a
 BadData a -> a
 JsonParseError -> "json parse error."
 InputErr -> "input error."
 InternalError -> "internal error."
 Unknown -> "unknown."
