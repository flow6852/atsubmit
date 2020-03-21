{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Server.GetPage where

import Lib

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Text.HTML.DOM
import Text.XML.Cursor


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

getContestInfo :: ReqAtSubmit -> Contest -> IO (Either (Int, T.Text) (V.Vector T.Text, ResAtSubmit)) -- (question name, Responce)
getContestInfo msg ud = case (cname msg, qname msg) of
 (Just cm, Nothing) -> let contesturl = V.foldl1 T.append ["https://atcoder.jp/contests/", cm, "/tasks"] in do
  res <- getRequestWrapper contesturl (cookie ud)
  if getResponseStatus res /= status200 then return $ Left (404, "tasks not found.")
  else let base = (fromDocument.parseLBS.getResponseBody) res in 
   return $ Right ((V.fromList.quests) base, createResAtStatus 200 "end")
 where
  quests :: Cursor -> [T.Text]
  quests = (Prelude.map (T.takeWhileEnd (/='/')).Prelude.concatMap (attribute "href").
           ($// attributeIs "class" "text-center no-break" &// element "a"))

getPageInfo :: ReqAtSubmit -> Contest -> IO (Either (Int, T.Text) (Question, ResAtSubmit))
getPageInfo msg ud = case (cname msg, qname msg) of
 (Just cm, Just qm) -> let questurl = V.foldl1 T.append ["https://atcoder.jp/contests/", cm, "/tasks/", qm] in
  if V.elem questurl ((V.map qurl.questions) ud) then return (Left (405, "already get.")) else do
   res <- getRequestWrapper questurl (cookie ud)
   if getResponseStatus res /= status200 then return $  Left (404, "question not found.")
   else let fname = T.unpack (V.foldl1 T.append [userdir msg, "/", qm, ".html"]) in
    TIO.writeFile fname ((rewriteHtml.decodeUtf8.BSL.toStrict.getResponseBody) res) >> return (Right (
     createQuestion questurl ((questionIO.fromDocument.parseLBS.getResponseBody) res)
     , createResAtSubmit 200 "get html and test case." [[qm]]))
 _ -> return $ Left (400, "set contest name and question name")
 where 
  questionIO :: Cursor -> V.Vector (T.Text, T.Text)
  questionIO cursor = do
   let cs = Prelude.map child $ cursor $// attributeIs "class" "col-sm-12" &// element "section" &// element "pre" 
   V.fromList.ioZip $ Prelude.map chnl.concatMap content $ Prelude.concat.Prelude.tail $ cs
  ioZip :: [T.Text] -> [(T.Text, T.Text)]
  ioZip (i:o:lists) 
   | T.null i || T.singleton '\n' == i ||  T.null o || T.singleton '\n' == o  = []
   | Prelude.null lists || (T.null.Prelude.head) lists                        = [(i, o)] 
   | otherwise                                                                = (i, o):ioZip lists
  chnl :: T.Text -> T.Text
  chnl = T.dropWhile (\x -> (x==' ')||(x=='\n')).T.dropWhileEnd (\x -> (x==' ')||(x=='\n')).T.replace (T.pack "\r\n") (T.pack "\n")
  rewriteHtml :: T.Text -> T.Text
  rewriteHtml = T.replace "/public/js/lib/jquery-1.9.1.min.js" ajax.T.replace "//cdn" "https://cdn"
  ajax = "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" 
