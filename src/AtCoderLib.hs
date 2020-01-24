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

server :: Socket -> UserData -> IO (Bool, UserData)
server sock user = do
 msg <- NSBS.recv sock 1024 
 if (BS.null msg) then return (True, user) else do
  case (T.unpack.decodeUtf8) msg of
   "stop"   -> return (True, user)
   "get"    -> atGetPage user
   "show"   -> atShowPage user
   "submit" -> atSubmit user
   "test"   -> atTest user
   _        -> return (False, user)

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

atLogin :: UserData -> IO (Bool,UserData)
atLogin user = do 
 next <- getCookieAndCsrfToken user
 print next
 return (False, next)

atGetPage :: UserData -> IO (Bool,UserData)
atGetPage user = do
 putStrLn "atGetPage"
 return (False, user)

atShowPage :: UserData -> IO (Bool,UserData)
atShowPage user = do
 putStrLn "atShowPage"
 return (False, user)

atSubmit :: UserData -> IO (Bool,UserData)
atSubmit user = do
 putStrLn "atSubmit"
 return (False, user)

atTest :: UserData -> IO (Bool,UserData)
atTest user = do
 putStrLn "atTest"
 return (False, user)
