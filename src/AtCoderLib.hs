{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module AtCoderLib where

import Lib

import Data.Text.Encoding
import Data.ByteString.Lazy.Internal
import qualified Network.Socket.ByteString as NSBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy
import Control.Monad
import qualified Data.Vector as V
import qualified Text.XML.Cursor as TXC
import qualified Text.HTML.DOM as THD
import System.Directory
import Control.Applicative
import Control.Exception
import qualified Data.Aeson as DA

server :: Socket -> Contest -> IO (Bool, Contest)
server sock contests = do
 json <- fromStrict <$> NSBS.recv sock 1024 
 case DA.decode json of
  Nothing -> NSBS.send sock ((encodeUtf8.T.pack) "parsing error")  >> return (False, contests)
  Just x  -> do
   let (func, retb) =  case (T.unpack.subcmd) x of
                            "stop"   -> (atLogout, True)
                            "get"    -> (atGetPage, False)
                            "show"   -> (atShowPage, False)
                            "submit" -> (atSubmit, False)
                            "test"   -> (atTest, False)
                            "login"  -> (atLogin, False)
                            "result" -> (atResult, False)
                            "help"   -> (atHelp, False)
                            _        -> (notDo, False)
   (resStr, retc) <- func contests x
                     `catch` (\e -> return ((T.pack.displayException) (e :: SomeException), contests))
   NSBS.send sock $ encodeUtf8 resStr
   return (retb, retc)
    where
     notDo :: AtFunc
     notDo c m = return (T.empty, c)

client :: [T.Text] -> Socket -> IO()
client msg sock = do
 cwd <- T.pack <$> getCurrentDirectory
 NSBS.send sock $ toStrict.DA.encode $ createAtSubmit msg cwd
 res <- NSBS.recv sock 1024
 BSC.putStrLn res

atLogin :: AtFunc
atLogin contests msg = do 
 !next <- getAtKeys >>= \[user, pass] -> getCookieAndCsrfToken (T.pack user) (T.pack pass)
 return (T.pack "login", next)

atGetPage :: AtFunc 
atGetPage contests msg = case cname msg of
 Nothing -> return ("command error.", contests)
 Just cm -> case V.find ((== cm).T.takeWhileEnd (/='/').qurl) $ questions contests of
   Just a -> return (T.pack "already get", contests)
   Nothing -> do
    quest <- getPageInfo msg contests
    return $ if quest == nullQuestion then (T.pack "not found", contests) 
             else (T.pack "get url,io", contests {questions = V.snoc (questions contests) quest})

atShowPage :: AtFunc
atShowPage contests msg = case qname msg of
 Nothing -> return ((atAllShow.questions) contests, contests)
 Just qm -> do 
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  let showPage = case mquest of Nothing -> T.pack "not found"
                                Just a  -> T.append "htmlfile\n" $ T.append ((T.pack.htmlpath) a) $ V.foldl1 T.append $ V.map showMsg $ qio a
  return (showPage, contests)
   where
    showMsg :: (T.Text, T.Text) -> T.Text
    showMsg q = V.foldl1 T.append ["\ninput\n", fst q, "\noutput\n", snd q]

atAllShow :: V.Vector Question  -> T.Text
atAllShow q = if V.null q then T.empty else T.append ((T.takeWhileEnd (/='/').qurl.V.head) q) 
                                          $ T.append (T.singleton '\n') $ atAllShow.V.tail $ q

atSubmit :: AtFunc
atSubmit contests msg = do
 postSubmit msg contests
 let submitStatus = T.pack "submit"
 return (submitStatus, contests)

atResult :: AtFunc
atResult contests msg = case cname msg of
 Just cm -> do
  res <- getContestResult cm contests
  return (res, contests)
 Nothing -> if V.null (questions contests) then return ("nothing", contests) else do
  res <- T.unlines <$> loop ((rmDup.V.map (T.takeWhile (/='_').T.takeWhileEnd (/='/').qurl)) (questions contests)) contests
  return (res, contests)
   where
    loop :: V.Vector T.Text -> Contest -> IO [T.Text]
    loop quest cont = if V.null quest then return [] else do
     res <- getContestResult (V.head quest) cont
     bef <- loop (V.tail quest) cont
     return $ T.append "===== " (T.append (V.head quest) $ T.append " =====\n" res):bef

atTest :: AtFunc
atTest contests msg = case (qname msg, file msg) of
 (Just qm, Just fm) -> do
  home <- getHomeDirectory
  TIO.readFile (T.unpack (T.append (userdir msg) (T.append (T.singleton '/') fm))) >>= TIO.writeFile (home ++ mainfile)
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  result <- case mquest of Nothing -> return "not found"
                           Just a  -> testLoop (qio a) home 1
  return (result, contests)
 _  -> return (T.pack "command error.", contests)
 where
  mainfile = "/.cache/atsubmit/src/source.txt"

atLogout :: AtFunc
atLogout contests msg = postLogout contests >>= \x -> return (x, nullContest)

atHelp :: AtFunc
atHelp contests msg = TIO.readFile helpFile >>= \x -> return (x, contests)
