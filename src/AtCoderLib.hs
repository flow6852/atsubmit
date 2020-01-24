{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module AtCoderLib where

import Lib

import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Data.Text.Encoding
import Data.ByteString.Lazy.Internal
import qualified Data.Text as T
import Network.Socket
import qualified Network.Socket.ByteString as NSBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import Control.Monad
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Text.XML.Cursor as TXC
import qualified Text.HTML.DOM as THD
import Control.Applicative

type AtFunc = UserData -> V.Vector ContestData -> V.Vector T.Text -> IO (T.Text, UserData)

server :: Socket -> UserData -> V.Vector ContestData -> IO (Bool, UserData)
server sock user contests = do
 msg <- decodeUtf8 <$> NSBS.recv sock 1024 
 if T.null msg then return (True, user) else do
  let (func, retb) =  case (T.unpack.Prelude.head.T.words) msg of
                           "stop"   -> (notDo, True)
                           "get"    -> (atGetPage, False)
                           "show"   -> (atShowPage, False)
                           "submit" -> (atSubmit, False)
                           "test"   -> (atTest, False)
                           "login"  -> (atLogin, False)
                           _        -> (notDo, False)
  (resStr, retu)<- func user contests ((V.fromList.Prelude.tail.T.words) msg)
  NSBS.send sock $ encodeUtf8 msg 
  return (retb, retu)
   where
    notDo :: AtFunc
    notDo u c m = return (T.empty, u)

client :: [T.Text] -> Socket -> IO()
client msg sock = do
 NSBS.send sock $ (encodeUtf8.T.unwords) msg
 res <- NSBS.recv sock 1024
 putStr "Rec: "
 BSC.putStrLn res

getCookieAndCsrfToken :: UserData -> IO UserData
getCookieAndCsrfToken userdata = do
 fstreq <- parseRequest "https://atcoder.jp/login"
 fstmng <- newManager tlsManagerSettings
 fstres <- Network.HTTP.Conduit.httpLbs fstreq fstmng
 let csrf_tkn = (getCsrfToken.decodeUtf8.BSL.toStrict.getResponseBody) fstres
 let fstcookie = getResponseHeader hSetCookie fstres
 responce <- do
  req <- (setRequestHeader hCookie fstcookie.setRequestMethod "POST") <$> parseRequest "https://atcoder.jp/login"
  let postReq = urlEncodedBody [("username", (encodeUtf8.username) userdata),
                                ("password", (encodeUtf8.password) userdata),
                                ("csrf_token",encodeUtf8 csrf_tkn)] req
  manager <- newManager tlsManagerSettings
  Network.HTTP.Conduit.httpLbs postReq manager
 return userdata {csrf_token = csrf_tkn, cookie = getResponseHeader hSetCookie responce}
 where
  getCsrfToken :: T.Text -> T.Text
  getCsrfToken body = T.takeWhile (/= '\"') $ snd $ T.breakOnEnd (T.pack "value=\"") body

getPageInfo :: V.Vector T.Text -> UserData -> IO (ContestData)
getPageInfo msg user = do -- msg = [contestname]
 let cname = T.takeWhile (/='_') $ V.head msg
     questurl = T.append (T.pack "https://atcoder.jp/contests/") $ T.append cname $ T.append (T.pack "/tasks") $ V.head msg
 r <- parseRequest $ T.unpack questurl
 let req = setRequestHeader hCookie (cookie user) r
 mng <- newManager tlsManagerSettings
 resBody <- parseLBS.getRequestBody <$> Network.HTTP.Conduit.httpLbs req mng
 let questext = fromDocument resBody
 return nullContestData
  where 
   getQuestionText :: Cursor -> T.Text
   getQuestionText cursor = do
    
 
atLogin :: AtFunc
atLogin user contests msg = do 
 next <- getCookieAndCsrfToken user
 print next
 return (T.pack "login", next)

atGetPage :: AtFunc 
atGetPage user contests msg = do
 getPageInfo msg user
 putStrLn "atGetPage"
 return (T.pack "get page", user)

atShowPage :: AtFunc
atShowPage user contests msg = do
 putStrLn "atShowPage"
 let showPage = T.pack "show"
 return (showPage, user)

atSubmit :: AtFunc
atSubmit user contests msg = do
 putStrLn "atSubmit"
 return (T.pack "submit", user)

atTest :: AtFunc
atTest user contests msg = do
 let result = T.pack "test"
 putStrLn "atTest"
 return (result, user)
