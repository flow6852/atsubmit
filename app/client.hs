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
import System.Exit

sockpath = "/.local/lib/atsubmit/atsubmit.sock"

main :: IO ()
main = do
 path <- (++ sockpath) <$> getEnv "HOME"
 wd <- getCurrentDirectory
 arg <- Prelude.map T.pack <$> getArgs
 case arg of 
  ["login"] -> login path `catch` \(e :: SHelperException) -> exceptionText e
  ["qget", qn] -> qget path qn wd `catch` \(e :: SHelperException) -> exceptionText e
  ["cget", cn] -> cget path cn wd `catch` \(e :: SHelperException) -> exceptionText e
  ["test", qn, fn] -> test path (T.unpack fn) qn wd `catch` \(e :: SHelperException) -> exceptionText e
  ["show", qn] -> Main.show path qn `catch` \(e :: SHelperException) -> exceptionText e
  ["print"] -> Main.print path `catch` \(e :: SHelperException) -> exceptionText e
  ["submit", qn, fn] -> submit path (T.unpack fn) qn wd `catch` \(e :: SHelperException) -> exceptionText e
  ["debug", src, din] -> debug path (T.unpack src) (T.unpack din) wd `catch` \(e :: SHelperException) -> exceptionText e
  ["result", cn] -> result path cn `catch` \(e :: SHelperException) -> exceptionText e
  ["stop"] -> stop path `catch` \(e :: SHelperException) -> exceptionText e
  ["logout"] -> logout path `catch` \(e :: SHelperException) -> exceptionText e
  _ -> exceptionText (BadData "command error")

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
 cGetPrint res
  where
   cGetPrint :: V.Vector QName -> IO() 
   cGetPrint qn = if V.null qn then return ()
                  else case V.head qn of (QName pr) -> TIO.putStrLn pr >> cGetPrint (V.tail qn)

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

exceptionText :: SHelperException -> IO ()
exceptionText e = case e of
 FailLogin -> TIO.putStrLn "login failed." >> exitWith (ExitFailure 1)
 AlreadyGet -> TIO.putStrLn "already get." >> exitWith (ExitFailure 2)
 NotExistsContest -> TIO.putStrLn "contest not exist." >> exitWith (ExitFailure 3)
 QuestionNotFound -> TIO.putStrLn "question not found." >> exitWith (ExitFailure 4)
 NotGetQuestion (QName a) -> TIO.putStrLn (T.append "not get : " a) >> exitWith (ExitFailure 5)
 BadData a -> TIO.putStrLn a >> exitWith (ExitFailure 6)
 JsonParseError -> TIO.putStrLn "json parse error." >> exitWith (ExitFailure 7)
 InputErr -> TIO.putStrLn "input error." >> exitWith (ExitFailure 8)
 InternalError -> TIO.putStrLn "internal error." >> exitWith (ExitFailure 9)
 Unknown -> TIO.putStrLn "unknown." >> exitWith (ExitFailure 10)
