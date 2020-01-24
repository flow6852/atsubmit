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
import qualified Data.Text.IO as TIO
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
import Text.XML.Cursor
import Text.HTML.DOM as H
import Turtle

type AtFunc = Contest -> V.Vector T.Text -> IO (T.Text, Contest)

dockershell = ""

server :: Socket -> Contest -> IO (Bool, Contest)
server sock contests = do
 msg <- decodeUtf8 <$> NSBS.recv sock 1024 
 if T.null msg then return (True, contests) else do
  let (func, retb) =  case (T.unpack.Prelude.head.T.words) msg of
                           "stop"   -> (notDo, True)
                           "get"    -> (atGetPage, False)
                           "show"   -> (atShowPage, False)
                           "submit" -> (atSubmit, False)
                           "result" -> (atResult, False)
                           "test"   -> (atTest, False)
                           "login"  -> (atLogin, False)
                           _        -> (notDo, False)
  (resStr, retc) <- func contests ((V.fromList.Prelude.tail.T.words) msg)
  NSBS.send sock $ encodeUtf8 resStr
  return (retb, retc)
   where
    notDo :: AtFunc
    notDo c m = return (T.empty, c)

client :: [T.Text] -> Socket -> IO()
client msg sock = do
 NSBS.send sock $ (encodeUtf8.T.unwords) msg
 res <- NSBS.recv sock 1024
 putStr "Rec: "
 BSC.putStrLn res

getCookieAndCsrfToken :: T.Text -> T.Text -> IO Contest
getCookieAndCsrfToken un pw = do
 fstreq <- parseRequest "https://atcoder.jp/login"
 fstmng <- newManager tlsManagerSettings
 fstres <- Network.HTTP.Conduit.httpLbs fstreq fstmng
 let csrf_tkn = (getCsrfToken.decodeUtf8.BSL.toStrict.getResponseBody) fstres
 let fstcookie = getResponseHeader hSetCookie fstres
 responce <- do
  req <- (setRequestHeader hCookie fstcookie.setRequestMethod "POST") <$> parseRequest "https://atcoder.jp/login"
  let postReq = urlEncodedBody [("username", encodeUtf8 un),
                                ("password", encodeUtf8 pw),
                                ("csrf_token",encodeUtf8 csrf_tkn)] req
  manager <- newManager tlsManagerSettings
  Network.HTTP.Conduit.httpLbs postReq manager
 return $ createContest V.empty (getResponseHeader hSetCookie responce) csrf_tkn
 where
  getCsrfToken :: T.Text -> T.Text
  getCsrfToken body = T.takeWhile (/= '\"') $ snd $ T.breakOnEnd (T.pack "value=\"") body

getPageInfo :: V.Vector T.Text -> Contest -> IO (Question)
getPageInfo msg ud = do -- msg = [contestname]
 let cname = T.takeWhile (/='_') $ V.head msg
     questurl = T.append (T.pack "https://atcoder.jp/contests/") $ T.append cname $ T.append (T.pack "/tasks/") $ V.head msg
 r <- parseRequest $ T.unpack questurl
 let req = setRequestHeader hCookie (cookie ud) r
 mng <- newManager tlsManagerSettings
 resBody <- parseLBS.getResponseBody <$> Network.HTTP.Conduit.httpLbs req mng
 return $ createQuestion questurl ((questionIO.fromDocument) resBody)
  where 
   questionIO :: Cursor -> V.Vector (T.Text, T.Text)
   questionIO cursor = do
    let cs = cursor $// attributeIs "class" "col-sm-12" &// element "section" &// element "pre" 
    V.fromList.ioZip $ Prelude.concat.Prelude.map content $ Prelude.concat.Prelude.tail $ Prelude.map child cs
   ioZip :: [T.Text] -> [(T.Text, T.Text)]
   ioZip (i:o:lists) = if T.null i || (T.pack "\r\n") == i ||  T.null o || (T.pack "\r\n") == o then [] else 
                       if (Prelude.null) lists || (T.null.Prelude.head) lists then [(i, o)] else (i, o):ioZip lists
   ioZip _           = []

postSubmit :: V.Vector T.Text -> Contest -> IO ()
postSubmit msg ud = do
 let cname = T.takeWhile (/='_') $ V.head msg
     questurl = T.append (T.pack "https://atcoder.jp/contests/") $ T.append cname (T.pack "/submit")
 source <- TIO.readFile $ T.unpack $ msg V.! 1
 r <- parseRequest $ T.unpack questurl
 let req = setRequestHeader hCookie (cookie ud) r
 response <- do
  let postReq = urlEncodedBody [ ("data.TaskScreenName",(encodeUtf8.V.head) msg)
                               , ("data.LanguageId", (encodeUtf8.T.pack) "3014") -- this id is only Haskell
                               , ("sourceCode", encodeUtf8 source)
                               , ("csrf_token", (encodeUtf8.csrf_token) ud)] req
  manager <- newManager tlsManagerSettings
  Network.HTTP.Conduit.httpLbs postReq manager
 return ()
 
atLogin :: AtFunc
atLogin contests msg = do 
 next <- (getAtKeys >>= \[user, pass] -> getCookieAndCsrfToken (T.pack user) (T.pack pass))
 return (T.pack "login", next)

atGetPage :: AtFunc 
atGetPage contests msg = do
 quest<- getPageInfo msg contests
 return (T.pack "get url,io", contests {questions = V.snoc (questions contests) quest})

atShowPage :: AtFunc
atShowPage contests msg = do
 let mquest = V.find ((==(V.head msg)).T.takeWhileEnd (/='/').qurl) $ questions contests
 let showPage = case mquest of Nothing -> T.pack "not found"
                               Just a  -> V.foldl1 T.append $ V.map (\x -> T.append (fst x) (snd x)) $ qio a
 return (showPage, contests)

atResult :: AtFunc
atResult contests msg = return (T.pack "result", contests)

atSubmit :: AtFunc
atSubmit contests msg = do
 postSubmit msg contests
 let submitStatus = T.pack "submit"
 return (submitStatus, contests)

atTest :: AtFunc
atTest contests msg = do
 inshell (T.pack )
 let result = T.pack "test"
 return (result, contests)
