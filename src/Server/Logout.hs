{-# LANGUAGE OverloadedStrings #-}
module Server.Logout where

import Lib

import qualified Data.Text as T
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.Directory

atLogout :: AtFunc
atLogout contests msg = do
 result <- postLogout contests
 removeFile (((T.unpack.homedir) contests) ++ cookieFile)
 return $ case result of Left x  -> Left x
                         Right x -> Right (contests, x) 
 
postLogout :: Contest -> IO (Either (Int, T.Text) ResAtSubmit)
postLogout ud = case cookie ud of
 [] -> return $ Left (404, "don't login.")
 c  -> do
  res <- postRequestWrapper "https://atcoder.jp/logout" c [("csrf_token", csrf_token ud)]
  if getResponseStatus res /= status200 
   then return $ Left (403, (T.append "fail logout. status code :" ((T.pack.show.getResponseStatusCode) res)))
   else return $ Right $ createResAtStatus 200 "accept logout"
