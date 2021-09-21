{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import Client
import Types

import qualified Data.Text as T
import Data.Functor
import Data.Char
import System.Environment
import System.Directory
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Control.Exception
import Control.Concurrent
import qualified Data.List as L
import System.Exit
import System.FilePath
import System.Console.Terminal.Size
import Data.Maybe
import GHC.Float

main :: IO ()
main = do
 path <- (</> sockpath) <$> getEnv "HOME"
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
  ["langid"] -> Main.langid path T.empty `catch` \(e :: SHelperException) -> exceptionExit e
  ["langid", lang] -> Main.langid path lang `catch` \(e :: SHelperException) -> exceptionExit e
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
 sendServer path (\x ->  evalSHelper (Test x (Source (wd, fn)) (QName qn)) x)
 TIO.putStrLn "test accept."

show :: FilePath -> T.Text -> IO ()
show path qn = do
 res <- sendServer path (evalSHelper (Show (QName qn)))
 showPrint res
  where
   showPrint :: Question -> IO ()
   showPrint quest = do
    TIO.putStrLn "===== url ====="
    TIO.putStrLn $ qurl quest
    TIO.putStrLn "===== question ====="
    TIO.putStrLn $ qsentence quest
    TIO.putStrLn "===== restriction ====="
    mapM_ TIO.putStrLn $ qrestriction quest
    TIO.putStrLn "===== input style ====="
    TIO.putStrLn $ fst.qio $ quest
    TIO.putStrLn "===== output style ====="
    TIO.putStrLn $ snd.qio $ quest
    TIO.putStrLn "=== input and output sample"
    showPrintSamples.V.toList $ qiosample quest
   showPrintSamples [] = return ()
   showPrintSamples (x:xs) = do
    TIO.putStrLn "===== input ====="
    TIO.putStrLn $ fst x
    TIO.putStrLn "===== output ====="
    TIO.putStrLn $ snd x
    showPrintSamples xs

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
 sid <- sendServer path (evalSHelper (Submit (Source (wd, fn)) (QName qn)))
 TIO.putStrLn "submit."
 TIO.putStrLn ""
 printUpdate $ V.foldl1 T.append [qn, alterTab, "WJ"]
 threadDelay $ 3000 * 1000
 resultRealtime path (T.takeWhile (/= '_') qn) qn  (Just sid)

debug :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
debug path src din wd = do
 debugin <- (\x -> if x then din else wd </> din) <$> doesFileExist din
 res <- sendServer path (evalSHelper (Types.Debug (Source (wd, src)) (DIn debugin)))
 case res of
  DAC (DOut dout) -> TIO.putStrLn dout
  DCE (Message message) -> TIO.putStrLn message
  DRE -> TIO.putStrLn "runtime error."
  DTLE -> TIO.putStrLn "time limit error."
  DIE -> TIO.putStrLn "internal error."

result :: FilePath -> T.Text -> IO ()
result path cn = do
 res <- sendServer path (evalSHelper (Result (CName cn) Nothing))
 let cres = case res of CResult tmp -> tmp
     sids = Prelude.map (\x -> if T.isInfixOf (T.singleton '/') (x!!8) || "WJ" == (x!!8)then Just (head x) else Nothing) $ Prelude.tail cres
 if all isNothing sids  then printResult cres
 else printResult cres >> threadDelay (1000 * 1000) >> putStrLn "" >> resultRealtime path cn (((!!2).Prelude.head.Prelude.tail) cres) ((toMaybeSid.Prelude.head) sids)
 where
  printResult =  mapM_ (TIO.putStrLn.T.intercalate " ")
  toMaybeSid (Just a) = Just (Sid a)

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
    (SHelperServerRequest (TestReq (Source (wd, src)) (QName qn)), SHelperOk res)
      -> V.foldl1 T.append ["Test ",T.pack (wd </> src), " " , qn , " : Ok"]
    (SHelperServerRequest (TestReq (Source (wd, src)) (QName qn)), SHelperErr res) 
      -> V.foldl1 T.append ["Test ",T.pack (wd </> src), " " , qn , " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest (SubmitReq (Source (wd, src)) (QName qn)), SHelperOk res)
      -> V.foldl1 T.append ["Submit ",T.pack (wd </>  src), " ", qn, " : Ok"]
    (SHelperServerRequest (SubmitReq (Source (wd, src)) (QName qn)), SHelperErr res) 
      -> V.foldl1 T.append ["Submit ",T.pack (wd </> src), " ", qn, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest (DebugReq (Source (wd, src)) (DIn din)), SHelperOk res)
      -> V.foldl1 T.append ["Debug ", T.pack (wd </> src), " ", T.pack din, " : Ok"]
    (SHelperServerRequest (DebugReq (Source (wd, src)) (DIn din)), SHelperErr res)
      -> V.foldl1 T.append ["Debug ", T.pack (wd </> src), " ", T.pack din, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest PrintReq, SHelperOk res) -> "Print : Ok"
    (SHelperServerRequest PrintReq, SHelperErr res) -> T.append "Print : Err " $ (fst.exceptionText) res
    (SHelperServerRequest (ShowReq (QName qn)), SHelperOk res)
      -> V.foldl1 T.append ["Show ", qn, " : Ok"]
    (SHelperServerRequest (ShowReq (QName qn)), SHelperErr res)
      -> V.foldl1 T.append ["Show ", qn, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest (ResultReq (CName cn) _), SHelperOk res)
      -> V.foldl1 T.append ["Result ", cn, " : Ok"]
    (SHelperServerRequest (ResultReq (CName cn) _), SHelperErr res)
      -> V.foldl1 T.append ["Result ", cn, " : Err ", (fst.exceptionText) res]
    (SHelperServerRequest LogoutReq, SHelperOk res) -> "Logout : Ok"
    (SHelperServerRequest LogoutReq, SHelperErr res) -> T.append "Logout : Err " $ (fst.exceptionText) res
    _ -> "other."

langid :: FilePath -> T.Text -> IO ()
langid path lang = do
 langids <- sendServer path (evalSHelper (LangId (Lang lang)))
 TIO.putStrLn "Language, Id"
 mapM_ (TIO.putStrLn.(\(LanguageId (Id a, Lang b)) -> V.foldl1 T.append [b, ", ", a])) langids

stop :: FilePath -> IO ()
stop path = do
 sendServer path (evalSHelper Stop)
 TIO.putStrLn "server stopped."

logout :: FilePath -> IO()
logout path = do
 sendServer path (evalSHelper Logout)
 TIO.putStrLn "logout accept."

resultRealtime :: FilePath -> T.Text -> T.Text -> Maybe Sid -> IO()
resultRealtime path cn qn sids = do
 res <- sendServer path (evalSHelper (Result (CName cn) sids))
 let cres = case res of CResult tmp -> T.takeWhile (/='<').(!!2).T.split (=='>').Prelude.head.Prelude.head $ tmp
 if T.isInfixOf "/" cres || cres == "WJ" then printRealtime qn cres >> threadDelay (1000 * interval res) >> resultRealtime path cn qn sids
                                         else printUpdate $ V.foldl1 T.append [qn, "    ", cres]

alterTab = "    "

printRealtime :: T.Text -> T.Text -> IO()
printRealtime qn cres = if T.isInfixOf "/" cres then do
  sharpWidth <- (\x -> x - T.length qn - T.length "[] " - T.length alterTab - T.length cres -1) <$> getWidth
  let [done, all] = Prelude.map (read.T.unpack.T.takeWhile isDigit).T.split (=='/') $ cres
      sharps      = T.replicate ((fromIntegral.floor :: Double -> Int) (int2Double sharpWidth * (done / all))) "#"
      tmp         = T.replicate ((fromIntegral.floor :: Double -> Int) (int2Double sharpWidth * ((all - done) / all))) "."
  dots <- getWidth >>= \x -> return $ T.replicate (T.length tmp - (T.length sharps + T.length tmp - sharpWidth)) "."
  printUpdate $ V.foldl1 T.append [qn, alterTab, "[", sharps, dots , "] ", cres]
 else printUpdate $ V.foldl1 T.append [qn, alterTab, T.takeWhile (/=' ') cres]

printUpdate :: T.Text -> IO()
printUpdate text = TIO.putStr "\^[[A" >> (getWidth >>= \x -> TIO.putStrLn (T.replicate x " ")) >> TIO.putStr "\^[[A" >> TIO.putStrLn text

getWidth :: IO Int
getWidth = size Data.Functor.<&> (\case Just Window {height = a, width = b} -> fromIntegral b :: Int)

interval res = case res of CResult tmp -> read.T.unpack.T.tail.T.takeWhile (/= ',').T.dropWhile (/=':').Prelude.head.Prelude.head $ tmp

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
 Unknown txt -> (T.append "unknown : " txt, 6)
