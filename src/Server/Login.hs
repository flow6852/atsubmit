{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Server.Login where

import Lib

import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import Text.HTML.DOM
import Text.XML.Cursor
import Network.HTTP.Simple
import Network.HTTP.Types.Header

atLogin :: AtFunc
atLogin contests msg = case (username msg, password msg) of
  (Just user, Just pass) -> getCookieAndCsrfToken user pass
  (Nothing, _)           -> return $ Left (405, "not set username")
  (_, Nothing)           -> return $ Left (405, "not set password")

getCookieAndCsrfToken :: T.Text -> T.Text -> IO (Either (Int, T.Text) (Contest, ResAtSubmit))
getCookieAndCsrfToken un pw = do
 fstres <- getRequestWrapper "https://atcoder.jp/login" []
 let csrf_tkn = (getCsrfToken.decodeUtf8.BSL.toStrict.getResponseBody) fstres
 let fstcke = getResponseHeader hSetCookie fstres
 responce <- postRequestWrapper "https://atcoder.jp/login" fstcke [ ("username", un), ("password", pw), ("csrf_token", csrf_tkn)]
 if (checkFailLogin.getResponseBody) responce
  then createContest V.empty [] [] >>= \x -> return $ Left (403, "fail login.")
  else createContest V.empty (getResponseHeader hSetCookie responce) csrf_tkn >>= 
       \x -> return $ Right (x, createResAtStatus 200 "accpet login")
 where
  getCsrfToken :: T.Text -> T.Text
  getCsrfToken body = T.replace "&#43;" "+".T.takeWhile (/= '\"').snd.T.breakOnEnd (T.pack "value=\"") $ body
  checkFailLogin :: BSL.ByteString -> Bool
  checkFailLogin = Prelude.null.($// attributeIs "class" "alert alert-success alert-dismissible col-sm-12 fade in").fromDocument.parseLBS
