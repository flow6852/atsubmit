{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module Lib where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import System.IO
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import Text.XML.Cursor
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import qualified Network.Socket.ByteString as NSBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Text.HTML.DOM as H
import Control.Exception
import Turtle
import qualified Turtle.Shell as TS
import qualified Control.Foldl as CF
import qualified Data.Aeson as DA

dockershell = "/.local/lib/atsubmit/docker_judge.sh"
helpFile = "/.local/share/man/atsubmit.man"
ajax = "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" 

type AtFunc = Contest -> ReqAtSubmit -> IO (Contest, ResAtSubmit)

data Question = Question { qurl :: T.Text -- question page's url
                         , qio :: V.Vector (T.Text, T.Text) -- input, output
                         } deriving (Show, Eq)

data Contest = Contest { questions :: V.Vector Question 
                       , cookie :: [BSC.ByteString]
                       , csrf_token :: T.Text
                       } deriving (Show, Eq)

data ReqAtSubmit = ReqAtSubmit { rcom :: T.Text -- raw command
                               , subcmd :: T.Text -- text
                               , cname :: Maybe T.Text -- contest name (ex abc120 
                               , qname :: Maybe T.Text -- question name (ex abc_a
                               , file :: Maybe T.Text
                               , userdir :: T.Text
                               } deriving (Show, Eq)

data ResAtSubmit = ResAtSubmit { resstatus :: Int    -- responce status 
                               , resmsg    :: T.Text -- responce message
                               , resresult :: [[T.Text]] -- [WA, output, testcase] or [input, output]
                               } deriving (Show, Eq)

instance DA.FromJSON ReqAtSubmit where
 parseJSON (DA.Object v) = ReqAtSubmit <$> (v DA..: "rcom")
                                       <*> (v DA..: "subcmd")
                                       <*> (v DA..:? "cname")
                                       <*> (v DA..:? "qname")
                                       <*> (v DA..:? "file")
                                       <*> (v DA..: "userdir")

instance DA.ToJSON ReqAtSubmit where
 toJSON (ReqAtSubmit rc sc cn qn f u ) = DA.object [ "rcom" DA..= rc
                                                   , "subcmd" DA..= sc
                                                   , "cname" DA..= cn
                                                   , "qname" DA..= qn
                                                   , "file" DA..= f
                                                   , "userdir" DA..= u]

instance DA.FromJSON ResAtSubmit where
 parseJSON (DA.Object v) = ResAtSubmit <$> (v DA..: "resstatus")
                                       <*> (v DA..: "resmsg")
                                       <*> (v DA..: "resresult")

instance DA.ToJSON ResAtSubmit where
 toJSON (ResAtSubmit rs rm rr) = DA.object [ "resstatus" DA..= rs
                                           , "resmsg" DA..= rm
                                           , "resresult" DA..= rr]

nullContest = Contest { questions = [], cookie = [], csrf_token = ""}
nullQuestion = Question { qurl = "", qio = []}
nullReqAtSubmit = ReqAtSubmit { rcom = "", subcmd = "", cname = Nothing, qname = Nothing, file = Nothing, userdir = ""}
nullResAtSubmit = ResAtSubmit { resstatus = 100, resmsg = "nothing", resresult = []}

createContest :: V.Vector Question -> [BSC.ByteString] -> T.Text -> Contest
createContest q c t = Contest { questions = q, cookie = c, csrf_token = t}

createQuestion :: T.Text -> V.Vector (T.Text, T.Text) -> Question
createQuestion url io = Question { qurl = url, qio = io}

createReqAtSubmit :: [T.Text] -> T.Text -> ReqAtSubmit
createReqAtSubmit r u = (listAtSubmit r) { rcom = T.unwords r, userdir = u}
 where
  listAtSubmit :: [T.Text] -> ReqAtSubmit
  listAtSubmit l = case Prelude.length l of
   1 -> nullReqAtSubmit { subcmd = l !! 0, cname = Nothing, qname = Nothing, file = Nothing} 
   2 -> nullReqAtSubmit { subcmd = l !! 0, cname = Just (T.takeWhile (/= '_') (l !! 1))
                        , qname = getQName (l !! 1), file = Nothing}
   3 -> nullReqAtSubmit { subcmd = l !! 0, cname = Just (T.takeWhile (/= '_') (l !! 1))
                        , qname = getQName (l !! 1), file = Just (l !! 2)}
   _ -> nullReqAtSubmit
  getQName :: T.Text -> Maybe T.Text
  getQName txt = case T.find (=='_') txt of
   Nothing -> Nothing
   Just a  -> Just txt

createResAtSubmit :: Int -> T.Text -> [[T.Text]] -> ResAtSubmit
createResAtSubmit rs rm rr = ResAtSubmit { resstatus = rs, resmsg = rm, resresult = rr}

createResAtStatus :: Int -> T.Text -> ResAtSubmit
createResAtStatus n rm = nullResAtSubmit { resstatus = n, resmsg = rm }

getAPIkeys :: [String] -> IO [String]
getAPIkeys [] = return []
getAPIkeys (m:messages) = do
 Prelude.putStr m 
 hFlush System.IO.stdout
 api <- Prelude.getLine 
 Prelude.putChar '\n'
 getAPIkeys messages >>= (\res -> return (api:res))

getAtKeys :: IO [String]
getAtKeys = do
 hSetEcho System.IO.stdin False
 System.IO.putStrLn "============== atcoder username and password ==============="
 apis <- getAPIkeys ["username : ", "password : "]
 hSetEcho System.IO.stdin True
 return apis

helpText :: IO T.Text
helpText = TIO.readFile helpFile

rmDup :: V.Vector T.Text -> V.Vector T.Text
rmDup = V.foldl (\seen x -> if V.elem x seen then seen else V.cons x seen) V.empty

takeNList :: Int -> BS.ByteString -> [BS.ByteString]
takeNList n base = BS.take n base:(if BS.length base < n then [] else takeNList n (BS.drop n base))

getRequestWrapper :: T.Text -> [BSC.ByteString] -> IO (Response BSL.ByteString)
getRequestWrapper url cke = do
 req <- if cke == [] then parseRequest (T.unpack url)
        else setRequestHeader hCookie cke <$> parseRequest (T.unpack url)
 mng <- newManager tlsManagerSettings
 Network.HTTP.Conduit.httpLbs req mng

postRequestWrapper :: T.Text -> [BSC.ByteString] -> [(BSC.ByteString, T.Text)] -> IO (Response BSL.ByteString)
postRequestWrapper url cke body = do
 req <- setRequestHeader hCookie cke <$> parseRequest (T.unpack url)
 let postReq = urlEncodedBody (Prelude.map (\(x,y) -> (x, encodeUtf8 y)) body) req
 mng <- newManager tlsManagerSettings
 Network.HTTP.Conduit.httpLbs postReq mng

getCookieAndCsrfToken :: T.Text -> T.Text -> IO (Contest, ResAtSubmit)
getCookieAndCsrfToken un pw = do
 fstres <- getRequestWrapper "https://atcoder.jp/login" []
 let csrf_tkn = (getCsrfToken.decodeUtf8.BSL.toStrict.getResponseBody) fstres
 let fstcke = getResponseHeader hSetCookie fstres
 responce <- postRequestWrapper "https://atcoder.jp/login" fstcke [ ("username", un), ("password", pw), ("csrf_token", csrf_tkn)]
 return $ if (checkFailLogin.getResponseBody) responce
          then (createContest V.empty [] []
               , createResAtStatus 403 "fail login.")
          else (createContest V.empty (getResponseHeader hSetCookie responce) csrf_tkn
               , createResAtStatus 200 "accpet login")
 where
  getCsrfToken :: T.Text -> T.Text
  getCsrfToken body = T.replace "&#43;" "+".T.takeWhile (/= '\"').snd.T.breakOnEnd (T.pack "value=\"") $ body
  checkFailLogin :: BSL.ByteString -> Bool
  checkFailLogin = Prelude.null.($// attributeIs "class" "alert alert-success alert-dismissible col-sm-12 fade in").fromDocument.parseLBS

getContestInfo :: ReqAtSubmit -> Contest -> IO (V.Vector T.Text, ResAtSubmit) -- (question name, Responce)
getContestInfo msg ud = case (cname msg, qname msg) of
 (Just cm, Nothing) -> let contesturl = V.foldl1 T.append ["https://atcoder.jp/contests/", cm, "/tasks"] in do
  res <- getRequestWrapper contesturl (cookie ud)
  if getResponseStatus res /= status200 then return ([], createResAtStatus 404 "tasks not found.")
  else let base = (fromDocument.parseLBS.getResponseBody) res in 
   return ((V.fromList.quests) base, createResAtStatus 200 "end")
 where
  quests :: Cursor -> [T.Text]
  quests = (Prelude.map (T.takeWhileEnd (/='/')).Prelude.concatMap (attribute "href").
           ($// attributeIs "class" "text-center no-break" &// element "a"))

getPageInfo :: ReqAtSubmit -> Contest -> IO (Question, ResAtSubmit)
getPageInfo msg ud = case (cname msg, qname msg) of
 (Just cm, Just qm) -> let questurl = V.foldl1 T.append ["https://atcoder.jp/contests/", cm, "/tasks/", qm] in
  if V.elem questurl ((V.map qurl.questions) ud) then return (nullQuestion, createResAtStatus 405 "already get.") else do
   res <- getRequestWrapper questurl (cookie ud)
   if getResponseStatus res /= status200 then return (nullQuestion, createResAtStatus 404 "question not found.")
   else let fname = T.unpack (V.foldl1 T.append [userdir msg, "/", qm, ".html"]) in
    TIO.writeFile fname ((rewriteHtml.decodeUtf8.BSL.toStrict.getResponseBody) res) >> return (
     createQuestion questurl ((questionIO.fromDocument.parseLBS.getResponseBody) res)
     , createResAtSubmit 200 "get html and test case." [[qm]])
 _ -> return (nullQuestion, createResAtStatus 400 "set contest name and question name")
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
        c = Prelude.concatMap content.lineNGet 4.Prelude.concatMap child $ cursor $// attributeIs "class" "table-responsive" 
                                                                                  &// element "td" 
                                                                                  &// element "a" -- [question, uname, details]
        result = Prelude.concatMap content.Prelude.concatMap child $ cursor $// attributeIs "class" "table-responsive"
                                                                            &// element "td"
                                                                            &// attributeIs "aria-hidden" "true"
    return $ zipLines subtime c result
   lineNGet :: Int -> [Cursor] -> [Cursor]
   lineNGet k l = if Prelude.length l >= k then Prelude.head l:lineNGet k (drop k l) else []
   zipLines :: [T.Text] -> [T.Text] -> [T.Text] -> [[T.Text]]
   zipLines [] [] [] = [] 
   zipLines [s] [c] [r] = [[s, c, r]]
   zipLines (s:t) (c:n) (r:e) = [s, c, r]:zipLines t n e

postSubmit :: ReqAtSubmit -> Contest -> IO ResAtSubmit
postSubmit msg ud = case (cname msg, qname msg, file msg) of
 (Just cm, Just qm, Just fm) -> do
  source <- TIO.readFile $ T.unpack $ V.foldl1 T.append [userdir msg, T.singleton '/', fm]
  let questurl = V.foldl1 T.append ["https://atcoder.jp/contests/", cm, "/submit"]
  res <- postRequestWrapper questurl (cookie ud) [ ("data.TaskScreenName", qm), ("data.LanguageId", "3014")
                                                 , ("sourceCode", source), ("csrf_token", csrf_token ud)]
  return $ createResAtStatus 200 "submit."
 _ -> return $ createResAtStatus 400 "set contest name, question name and file name for submit."
 
postLogout :: Contest -> IO ResAtSubmit
postLogout ud = do
 res <- postRequestWrapper "https://atcoder.jp/logout" (cookie ud) [("csrf_token", csrf_token ud)]
 return $ if getResponseStatus res /= status200 
  then createResAtStatus 403 (T.append "fali logout. status code :" ((T.pack.show.getResponseStatusCode) res))
  else createResAtStatus 200 "accept logout"

testLoop :: V.Vector (T.Text, T.Text) -> System.IO.FilePath -> Int -> IO [[T.Text]]
testLoop qs dir k = if V.null qs then return [] else do
 TIO.writeFile infile $ (fst.V.head) qs
 TIO.writeFile outfile $ (snd.V.head) qs
 ec <- shell (T.pack (dir ++ dockershell)) empty
 outres <- TIO.readFile outfile
 comp <- TIO.readFile compfile
 (out, next) <- (\x -> case ec of
                       ExitFailure 1 -> ([(T.pack.show) k, "CE", comp], [])
                       ExitFailure 2 -> ([(T.pack.show) k, "RE"], x)
                       ExitFailure _ -> ([(T.pack.show) k, "TLE"], x)
                       ExitSuccess   -> (if checkResult (T.lines outres) ((T.lines.snd.V.head) qs) then ([(T.pack.show) k, "AC"], x)
                                          else ([(T.pack.show) k, "WA", outres, (snd.V.head) qs], x))) <$> testLoop (V.tail qs) dir (k+1)
 return $ out:next
  where
   infile = dir ++ "/.cache/atsubmit/src/input.txt"
   outfile = dir ++ "/.cache/atsubmit/src/outres.txt"
   compfile = dir ++ "/.cache/atsubmit/src/comp.txt"
   checkResult :: [T.Text] -> [T.Text] -> Bool
   checkResult [] []           = True
   checkResult ([]:es) (ans)   = checkResult es ans
   checkResult (r:es) (a:ns)   = if r == a then checkResult es ns else False
   checkResult _ _             = False
