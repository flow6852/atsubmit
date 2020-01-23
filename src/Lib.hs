{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Lib where

import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Data.Text.Encoding
import Data.ByteString.Lazy.Internal
import qualified Data.Text as T

data Userdata = Userdata { un :: T.Text, pw :: T.Text, csrf_token :: ByteString} deriving(Show)

getCookieAndCsrfToken :: Userdata -> IO ([ByteString],Userdata)
getCookieAndCsrfToken userdata = do
 csrf_tkn <- getResponseHeader hSetCookie.httpLBS <$> parseRequest "https://atcoder.jp/login"
 responce <- do
  req <- parseRequest "https://atcoder.jp/login"
  let postReq = urlEncodedBody [("username", (encodeUtf8.un) userdata), ("password", (encodeUtf8.pw) userdata), ("csrf_token", csrf_tkn)]
  manager <- newManager tlsManagerSettings
  httpLBS postReq manager
 getResponseHeader hSetCookie responce >>= \x -> return (x, userdata {csrf_token = csrf_tkn} )


