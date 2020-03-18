{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Server.Result where

import Lib 

import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Text.HTML.DOM
import Text.XML.Cursor

atResult :: AtFunc
atResult contests msg = case cname msg of
 Just cm -> do
  res <- getContestResult cm contests
  return $ if Prelude.null res then Left (404, "not found.")
                               else Right (contests, createResAtSubmit 200 "accept getting result" res) -- only contest result
 Nothing -> if V.null (questions contests) then return (Left (405, "not get questions")) else do -- nothing questions
  res <- loop ((rmDup.V.map (T.takeWhile (/='_').T.takeWhileEnd (/='/').qurl)) (questions contests)) contests -- all result
  return $ Right (contests, createResAtSubmit 200 "accept getting results" res) -- all result
   where
    loop :: V.Vector T.Text -> Contest -> IO [[T.Text]]
    loop quest cont = if V.null quest then return [] else do
     res <- getContestResult (V.head quest) cont
     bef <- loop (V.tail quest) cont
     return $ ([V.head quest]:res) ++ bef

getContestResult :: T.Text -> Contest -> IO [[T.Text]] -- time, question, result
getContestResult cnt ud = if T.null cnt then return [] else do
 res <- getRequestWrapper (V.foldl1 T.append ["https://atcoder.jp/contests/", cnt, "/submissions/me"]) (cookie ud)
 if getResponseStatus res /= status200 then return []
 else resultIO.fromDocument.parseLBS.getResponseBody $ res
  where
   resultIO :: Cursor -> IO [[T.Text]]
   resultIO cursor = do
    let subtime = Prelude.concatMap content.Prelude.concatMap child $ cursor $// attributeIs "class" "table-responsive"
                                                                             &// attributeIs "class" "fixtime fixtime-second"
        c = Prelude.concatMap content.lineNGet (cOrP (cursor)).Prelude.concatMap child $ cursor $// attributeIs "class" "table-responsive"
                                                                                                &// element "td"
                                                                                                &// element "a" -- [question, uname, details]
        result = Prelude.concatMap content.Prelude.concatMap child $ cursor $// attributeIs "class" "table-responsive"
                                                                            &// element "td"
                                                                            &// attributeIs "aria-hidden" "true"
    return $ zipLines subtime c result
   cOrP :: Cursor -> Int 
   cOrP cs = do
    let ch = cs $// attributeIs "class" "table-responsive"
                &// element "td"
                &// attributeIs "class" "glyphicon glyphicon-search black"
    if Prelude.null ch then 3 else 4
   lineNGet :: Int -> [Cursor] -> [Cursor]
   lineNGet k l = if Prelude.length l >= k then Prelude.head l:lineNGet k (drop k l) else [] -- in contest, lineNGet 3 list else lineNGet 4
   zipLines :: [T.Text] -> [T.Text] -> [T.Text] -> [[T.Text]]
   zipLines [] [] [] = [] 
   zipLines [s] [c] [r] = [[s, c, r]]
   zipLines (s:t) (c:n) (r:e) = [s, c, r]:zipLines t n e


