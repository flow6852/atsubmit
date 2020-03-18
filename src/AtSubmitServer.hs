{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module AtSubmitServer where

import Lib
import UnixDomainSocket

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
import qualified Data.ByteString as BS
import Turtle
import qualified Turtle.Shell as TS
import qualified Control.Foldl as CF
import Control.Monad.Trans.Except

server :: Socket -> Contest -> IO (Bool, Contest)
server sock contests = do
 json <- fromStrict <$> recvMsg sock 1024 
 case DA.decode json :: Maybe ReqAtSubmit of
  Nothing -> sendMsg sock (errMsg "server : json parse error") 1024 >> return (False, contests)
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
   (retc, res) <- (\result -> case result of Left  (ei, em) -> (contests, createResAtStatus ei (T.append "server error : " em))
                                             Right y        -> y) <$> func contests x
   sendMsg sock ((toStrict.DA.encode) res) 1024
   return (retb, retc)
 where
  notDo :: AtFunc
  notDo c m = return $ Left (400, "sub command undefined.")
  errMsg :: T.Text -> BS.ByteString
  errMsg = toStrict.DA.encode.createResAtStatus 405

atLogin :: AtFunc
atLogin contests msg = case (username msg, password msg) of
  (Just user, Just pass) -> getCookieAndCsrfToken user pass
  (Nothing, _)           -> return $ Left (405, "not set username")
  (_, Nothing)           -> return $ Left (405, "not set password")

atGetPage :: AtFunc 
atGetPage contests msg = case (qname msg, cname msg) of
 (Just qm, Just cm) | V.elem qm (V.map (T.takeWhileEnd (/='/').qurl) (questions contests)) -> return $ Left (405, "already get.")
                    | otherwise -> getPageInfo msg contests >>= \result -> case result of 
                                    Left x       -> return $ Left x
                                    Right (x, y) -> return $ Right (contests {questions = V.snoc (questions contests) x}, y)
 (Nothing, Just cm) -> getContestInfo msg contests >>= \result -> case result of
  Left x -> return $ Left x
  Right (x, y) -> loop x msg contests >>= \y -> case y of 
   Left x -> return $ Left x
   Right qs -> return $ Right ( contests {questions = (questions contests) V.++ qs}
                              , createResAtSubmit 200 "get all" ((V.toList.V.map T.words) x))
 _                  -> return $ Left (400, "json error") -- json error
 where
  loop :: V.Vector T.Text -> ReqAtSubmit -> Contest -> IO (Either (Int, T.Text) (V.Vector Question))
  loop t m c = if V.null t then return (Right V.empty) else getPageInfo (m { qname = Just (V.head t)}) c >>= \result -> 
   case result of Left x       -> return $ Left x
                  Right (q, r) -> loop (V.tail t) m c >>= \next -> case next of Left x       -> return $ Left x
                                                                                Right quests -> return $ Right (V.cons q quests)
 

atShowPage :: AtFunc
atShowPage contests msg = case (cname msg, qname msg) of
 (Nothing, Nothing) -> return $ Right (contests, createResAtSubmit 200 "all show." [(atAllShow.questions) contests])
 (Just cm, Just qm) -> do
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  case mquest of Nothing -> return $ Left (405, "not get.")
                 Just a  -> return $ Right (contests, createResAtSubmit 200 "accept show" ((V.toList.V.map ltot.qio) a))
 _                  -> return $ Left (405, "nothing contest name")
 where
  ltot :: (T.Text, T.Text) -> [T.Text]
  ltot (a, b) = [a, b]

atAllShow :: V.Vector Question -> [T.Text]
atAllShow = V.toList.V.map (T.takeWhileEnd (/='/').qurl)

atSubmit :: AtFunc
atSubmit contests msg = postSubmit msg contests >>= \result -> (case result of Left x  -> return (Left x)
                                                                               Right x -> return (Right (contests, x)))

atResult :: AtFunc
atResult contests msg = case cname msg of
 Just cm -> do
  res <- getContestResult cm contests
  return $ if Prelude.null res then Left (404, "not found.")
                               else Right (contests, createResAtSubmit 200 "accept getting result" res) -- only contest result
 Nothing -> if V.null (questions contests) then return (Left (405, "not get questions")) else do -- nothing questions
  res <- loop ((rmDup.V.map (T.takeWhile (/='_').T.takeWhileEnd (/='/').qurl)) (questions contests)) contests -- all result
  return $ Right (contests, createResAtSubmit 200 "accept getting results" res) -- all result
   where
    loop :: V.Vector T.Text -> Contest -> IO [[T.Text]]
    loop quest cont = if V.null quest then return [] else do
     res <- getContestResult (V.head quest) cont
     bef <- loop (V.tail quest) cont
     return $ ([V.head quest]:res) ++ bef

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

atLogout :: AtFunc
atLogout contests msg = (\result -> case result of 
                                         Left x  -> Left x
                                         Right x -> Right (contests, x)) <$> postLogout contests

atHelp :: AtFunc
atHelp contests msg = getHomeDirectory >>= \dir -> TIO.readFile (dir ++ helpFile) >>=
                                            \x -> return $ Right (contests, createResAtSubmit 200 "help message" [[x]])
