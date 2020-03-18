{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Server.Submit where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

atSubmit :: AtFunc
atSubmit contests msg = postSubmit msg contests >>= \result -> (case result of Left x  -> return (Left x)
                                                                               Right x -> return (Right (contests, x)))

postSubmit :: ReqAtSubmit -> Contest -> IO (Either (Int, T.Text) ResAtSubmit)
postSubmit msg ud = case (cname msg, qname msg, file msg) of
 (Just cm, Just qm, Just fm) -> do
  source <- TIO.readFile $ T.unpack $ V.foldl1 T.append [userdir msg, T.singleton '/', fm]
  lang <- languageSelect (homedir ud) fm
  let questurl = V.foldl1 T.append ["https://atcoder.jp/contests/", cm, "/submit"]
  res <- postRequestWrapper questurl (cookie ud) [ ("data.TaskScreenName", qm), ("data.LanguageId", langid lang)
                                                 , ("sourceCode", source), ("csrf_token", csrf_token ud)]
  return $ Right $ createResAtStatus 200 "submit."
 _ -> return $ Left (400, "set contest name, question name and file name for submit.")

