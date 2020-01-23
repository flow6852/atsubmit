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

server :: Socket -> UserData -> IO Bool
server sock user = do
 msg <- NSBS.recv sock 1024 
 if (BS.null msg) then return True else do
  case (T.unpack.decodeUtf8) msg of
   "stop"   -> return True
   "get"    -> atGetPage >> return False
   "show"   -> atShowPage >> return False
   "submit" -> atSubmit >> return False
   "test"   -> atTest >> return False
   _        -> return False

client :: [T.Text] -> Socket -> IO()
client msg sock = do
 NSBS.send sock $ (encodeUtf8.T.unwords) msg
 res <- NSBS.recv sock 1024
 putStr "Rec: "
 BSC.putStrLn res

getCookieAndCsrfToken :: UserData -> IO (Cookie,UserData)
getCookieAndCsrfToken userdata = do
 fstreq <- parseRequest "https://atcoder.jp/login"
 fstmng <- newManager tlsManagerSettings
 fstres <- Network.HTTP.Conduit.httpLbs fstreq fstmng
 let csrf_tkn = (getCsrfToken.decodeUtf8.BSL.toStrict.getResponseBody) fstres
 responce <- do
  req <- parseRequest "https://atcoder.jp/login"
  let postReq = urlEncodedBody [("username", (encodeUtf8.username) userdata),
                                ("password", (encodeUtf8.password) userdata),
                                ("csrf_token",encodeUtf8 csrf_tkn)] req
  manager <- newManager tlsManagerSettings
  Network.HTTP.Conduit.httpLbs postReq manager
 getResponseHeader hSetCookie responce >>= \x -> return (x, userdata {csrf_token = csrf_tkn} )
 where
  getCsrfToken :: T.Text -> T.Text
  getCsrfToken body = T.takeWhile (/= '\"') $ snd $ T.breakOnEnd (T.pack "value=\"") body

atLogin :: IO()
atLogin = putStrLn "atLogin"

atGetPage :: IO()
atGetPage = putStrLn "atGetPage"

atShowPage :: IO()
atShowPage = putStrLn "atShowPage"

atSubmit :: IO()
atSubmit = putStrLn "atSubmit"

atTest :: IO()
atTest = putStrLn "atTest"
