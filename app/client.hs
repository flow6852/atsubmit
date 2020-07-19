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
  ["login"] -> login path `catch` \(e :: SHelperException) -> exceptionExit e
  ("qget":qn) -> qget path qn wd `catch` \(e :: SHelperException) -> exceptionExit e
  ("cget":cn) -> cget path cn wd `catch` \(e :: SHelperException) -> exceptionExit e
  ["test", qn, fn] -> test path (T.unpack fn) qn wd `catch` \(e :: SHelperException) -> exceptionExit e
  ["show", qn] -> Main.show path qn `catch` \(e :: SHelperException) -> exceptionExit e
  ["print"] -> Main.print path `catch` \(e :: SHelperException) -> exceptionExit e
  ["submit", qn, fn] -> submit path (T.unpack fn) qn wd `catch` \(e :: SHelperException) -> exceptionExit e
  ["debug", src, din] -> debug path (T.unpack src) (T.unpack din) wd `catch` \(e :: SHelperException) -> exceptionExit e
  ["result", cn] -> result path cn `catch` \(e :: SHelperException) -> exceptionExit e
  ["log"] -> Main.log path `catch` \(e :: SHelperException) -> exceptionExit e
  ["stop"] -> stop path `catch` \(e :: SHelperException) -> exceptionExit e
  ["logout"] -> logout path `catch` \(e :: SHelperException) -> exceptionExit e
  _ -> exceptionExit (BadData "command error")

login :: FilePath -> IO ()
login path = do
 [user, pass] <- map T.pack <$> getAtKeys 
 sendServer path (evalSHelper (Login (Username user) (Password pass)))
 TIO.putStrLn "login accept."

qget :: FilePath -> [T.Text] -> FilePath -> IO ()
qget path qn wd = do
 res <- sendServer path (evalSHelper (QGet ((V.fromList.Prelude.map QName) qn) (Userdir wd)))
 getResultPrint res

cget :: FilePath -> [T.Text] -> FilePath -> IO ()
cget path cn wd = do
 res <- sendServer path (evalSHelper (CGet ((V.fromList.Prelude.map CName) cn) (Userdir wd)))
 getResultPrint res

getResultPrint :: V.Vector GetResult -> IO()
getResultPrint [] = return ()
getResultPrint rs = do 
 case V.head rs of 
  GetResultOk (QName qn) -> TIO.putStrLn $ V.foldl1 T.append ["Ok : ", qn, "."]
  FromLocal (QName qn) -> TIO.putStrLn $ V.foldl1 T.append ["Ok (from local): ", qn, "."]
  AlreadyGet (QName qn) -> TIO.putStrLn $ V.foldl1 T.append ["Error : ", qn, " already get."]
  QuestionNotExist (QName qn) -> TIO.putStrLn $ V.foldl1 T.append ["Error : ", qn ," not exist."]
  ContestNotExist (CName cn) -> TIO.putStrLn $ V.foldl1 T.append ["Error : ", cn ," not exist."]
  Other -> TIO.putStrLn "Error : unknown"
 getResultPrint (V.tail rs)

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

log :: FilePath -> IO()
log path = do
 result <- sendServer path (evalSHelper Log)
 mapM_ logPrint result
  where
   logPrint :: RLog -> IO()
   logPrint (RLog l) = TIO.putStrLn $ case l of
    (SHelperServerRequest (LoginReq un ps), SHelperOk res) -> "Login : Ok"
    (SHelperServerRequest (LoginReq un ps), SHelperErr res) -> T.append "Login : Err " $ (fst.exceptionText) res
    (SHelperServerRequest (QGetReq req (Userdir ud)), SHelperOk res) 
      -> V.foldl1 T.append ["QGet ", (T.intercalate " ".V.toList.V.map (\(QName a) -> a)) req, " : Ok"]
    (SHelperServerRequest (QGetReq req (Userdir ud)), SHelperErr res)
      -> V.foldl1 T.append ["QGet ", (T.intercalate " ".V.toList.V.map (\(QName a) -> a)) req, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest (CGetReq req (Userdir ud)), SHelperOk res)
      -> V.foldl1 T.append  ["CGet ", (T.intercalate " ".V.toList.V.map (\(CName a) -> a)) req, " : Ok"]
    (SHelperServerRequest (CGetReq req (Userdir ud)), SHelperErr res)
      -> V.foldl1 T.append  ["CGet ", (T.intercalate " ".V.toList.V.map (\(CName a) -> a)) req, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest (TestReq (Source src) (QName qn)), SHelperOk res)
      -> V.foldl1 T.append ["Test ",T.pack src, " " , qn , " : Ok"]
    (SHelperServerRequest (TestReq (Source src) (QName qn)), SHelperErr res) 
      -> V.foldl1 T.append ["Test ",T.pack src, " " , qn , " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest (SubmitReq (Source src) (QName qn)), SHelperOk res)
      -> V.foldl1 T.append ["Submit ",T.pack src, " ", qn, " : Ok"]
    (SHelperServerRequest (SubmitReq (Source src) (QName qn)), SHelperErr res) 
      -> V.foldl1 T.append ["Submit ",T.pack src, " ", qn, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest (DebugReq (Source src) (DIn din)), SHelperOk res)
      -> V.foldl1 T.append ["Debug ", T.pack src, " ", T.pack din, " : Ok"]
    (SHelperServerRequest (DebugReq (Source src) (DIn din)), SHelperErr res)
      -> V.foldl1 T.append ["Debug ", T.pack src, " ", T.pack din, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest PrintReq, SHelperOk res) -> "Print : Ok"
    (SHelperServerRequest PrintReq, SHelperErr res) -> T.append "Print : Err " $ (fst.exceptionText) res
    (SHelperServerRequest (ShowReq (QName qn)), SHelperOk res)
      -> V.foldl1 T.append ["Show ", qn, " : Ok"]
    (SHelperServerRequest (ShowReq (QName qn)), SHelperErr res)
      -> V.foldl1 T.append ["Show ", qn, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest (ResultReq (CName cn)), SHelperOk res)
      -> V.foldl1 T.append ["Result ", cn, " : Ok"]
    (SHelperServerRequest (ResultReq (CName cn)), SHelperErr res)
      -> V.foldl1 T.append ["Result ", cn, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest LogoutReq, SHelperOk res) -> "Logout : Ok"
    (SHelperServerRequest LogoutReq, SHelperErr res) -> T.append "Logout : Err " $ (fst.exceptionText) res
    _ -> "other."

stop :: FilePath -> IO ()
stop path = do
 sendServer path (evalSHelper Stop)
 TIO.putStrLn "server stopped."

logout :: FilePath -> IO()
logout path = do
 sendServer path (evalSHelper Logout)
 TIO.putStrLn "logout accept."

exceptionExit :: SHelperException -> IO ()
exceptionExit e = TIO.putStrLn ((fst.exceptionText) e) >> exitWith (ExitFailure ((snd.exceptionText) e))

exceptionText :: SHelperException -> (T.Text, Int)
exceptionText e = case e of
 FailLogin -> ("login failed.", 1)
 NotGetQuestion (QName a) -> (T.append "don't get : " a,2)
 BadData a -> (a, 3)
 JsonParseError -> ("json parse error.", 4)
 InputErr -> ("input error.", 4)
 InternalError -> ("internal error.", 5)
 Unknown -> ("unknown.", 6)
