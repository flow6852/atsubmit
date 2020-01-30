{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module AtSubmitServer where

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
 case DA.decode json :: Maybe ReqAtSubmit of
  Nothing -> NSBS.send sock ((encodeUtf8.T.pack) "server : json parse error")  >> return (False, contests)
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
   (retc, res) <- func contests x `catch`
     (\e -> return (contests, createResAtStatus 400 (T.append "server error : " ((T.pack.displayException) (e :: SomeException)))))
   NSBS.send sock $ toStrict.DA.encode $ res
   return (retb, retc)
    where
     notDo :: AtFunc
     notDo c m = return (c, createResAtStatus 400 "sub command undefined.")

atLogin :: AtFunc
atLogin contests msg = getAtKeys >>= \[user, pass] -> getCookieAndCsrfToken (T.pack user) (T.pack pass)

atGetPage :: AtFunc 
atGetPage contests msg = case qname msg of
 Nothing -> return (contests, createResAtStatus 400 "json error") -- json error
 Just qm -> if V.elem qm (V.map (T.takeWhileEnd (/='/').qurl) (questions contests)) 
            then return (contests, createResAtStatus 405 "already get.")
            else getPageInfo msg contests >>= \(x,y) -> return (contests {questions = V.snoc (questions contests) x}, y)

atShowPage :: AtFunc
atShowPage contests msg = case qname msg of
 Nothing -> return (contests, createResAtSubmit 200 "all show." [(atAllShow.questions) contests])
 Just qm -> do 
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  let showPage = case mquest of Nothing -> createResAtStatus 405 "not get."
                                Just a  -> createResAtSubmit 200 "accept show" ((V.toList.V.map ltot.qio) a)
  return (contests, showPage)
   where
    ltot :: (T.Text, T.Text) -> [T.Text]
    ltot (a, b) = [a,b]

atAllShow :: V.Vector Question  -> [T.Text]
atAllShow [] = []
atAllShow q  = ((T.takeWhileEnd (/='/').qurl.V.head) q):((atAllShow.V.tail) q)

atSubmit :: AtFunc
atSubmit contests msg = postSubmit msg contests >>= \x ->  return (contests, x)

atResult :: AtFunc
atResult contests msg = case cname msg of
 Just cm -> do
  res <- getContestResult cm contests
  return $ if Prelude.null res then (contests, createResAtStatus 404 "not found.")
                               else (contests, createResAtSubmit 200 "accept getting result" res) -- only contest result
 Nothing -> if V.null (questions contests) then return (contests, createResAtStatus 405 "not get questions") else do -- nothing questions
  res <- loop ((rmDup.V.map (T.takeWhile (/='_').T.takeWhileEnd (/='/').qurl)) (questions contests)) contests -- all result
  return (contests, createResAtSubmit 200 "accept getting results" res) -- all result
   where
    loop :: V.Vector T.Text -> Contest -> IO [[T.Text]]
    loop quest cont = if V.null quest then return [] else do
     res <- getContestResult (V.head quest) cont
     bef <- loop (V.tail quest) cont
     return $ ([V.head quest]:res) ++ bef

atTest :: AtFunc
atTest contests msg = case (qname msg, file msg) of
 (Just qm, Just fm) -> do
  home <- getHomeDirectory
  TIO.readFile (T.unpack (T.append (userdir msg) (T.append (T.singleton '/') fm))) >>= TIO.writeFile (home ++ mainfile)
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  (resint, result, resmsg) <- case mquest of Nothing -> return (405, [], "not get test case of questions") -- not getting
                                             Just a  -> testLoop (qio a) home 1 >>= \x -> return (200, x, "accept test")
  return (contests, createResAtSubmit resint resmsg result)
 _  -> return (contests, createResAtStatus 400 "set question name and file name") -- nothing question
 where
  mainfile = "/.cache/atsubmit/src/source.txt"

atLogout :: AtFunc
atLogout contests msg = postLogout contests >>= \x -> return (nullContest, x)

atHelp :: AtFunc
atHelp contests msg = getHomeDirectory >>= \dir -> TIO.readFile (dir ++ helpFile) 
                                       >>= \x -> return (contests, createResAtSubmit 200 "help message" [[x]])
