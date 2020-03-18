{-# LANGUAGE OverloadedStrings #-}
module Server.Logout where

import Lib

import qualified Data.Text as T
import Network.HTTP.Simple
import Network.HTTP.Types.Status

atLogout :: AtFunc
atLogout contests msg = (\result -> case result of 
                                         Left x  -> Left x
                                         Right x -> Right (contests, x)) <$> postLogout contests
 
postLogout :: Contest -> IO (Either (Int, T.Text) ResAtSubmit)
postLogout ud = do
 res <- postRequestWrapper "https://atcoder.jp/logout" (cookie ud) [("csrf_token", csrf_token ud)]
 if getResponseStatus res /= status200 
  then return $ Left (403, (T.append "fali logout. status code :" ((T.pack.show.getResponseStatusCode) res)))
  else return $ Right $ createResAtStatus 200 "accept logout"
