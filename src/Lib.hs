{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

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

dockershell = "~/.local/lib/atsubmit/docker_judge.sh"
helpFile = "~/.local/share/man/atsubmit.man"


type AtFunc = Contest -> AtSubmit -> IO (T.Text, Contest)

data Question = Question { qurl :: T.Text -- question page's url
                         , qio :: V.Vector (T.Text, T.Text) -- input, output
                         } deriving (Show, Eq)

data Contest = Contest { questions :: V.Vector Question 
                       , cookie :: [BSC.ByteString]
                       , csrf_token :: T.Text
                       } deriving (Show, Eq)

data AtSubmit = AtSubmit { rcom :: T.Text -- rawcommand
                         , subcmd :: T.Text
                         , cname :: Maybe T.Text -- contest name (ex abc120 
                         , qname :: Maybe T.Text -- question name (ex a
                         , file :: Maybe T.Text
                         , userdir :: T.Text
                         } deriving (Show, Eq)

instance DA.FromJSON AtSubmit where
 parseJSON (DA.Object v) = AtSubmit <$> (v DA..: "rcom")
                                    <*> (v DA..: "subcmd")
                                    <*> (v DA..:? "cname")
                                    <*> (v DA..:? "qname")
                                    <*> (v DA..:? "file")
                                    <*> (v DA..: "userdir")

instance DA.ToJSON AtSubmit where
 toJSON (AtSubmit rc sc cn qn f u) = DA.object [ "rcom" DA..= rc
                                               , "subcmd" DA..= sc
                                               , "cname" DA..= cn
                                               , "qname" DA..= qn
                                               , "file" DA..= f
                                               , "userdir" DA..= u]


nullContest = Contest { questions = V.empty, cookie = [], csrf_token = T.empty}
nullQuestion = Question { qurl = T.empty, qio = V.empty}
nullAtSubmit = AtSubmit { rcom = T.empty, subcmd = T.empty, cname = Nothing, qname = Nothing, file = Nothing, userdir = T.empty}

createContest :: V.Vector Question -> [BSC.ByteString] -> T.Text -> Contest
createContest q c t = Contest { questions = q, cookie = c, csrf_token = t}

createQuestion :: T.Text -> V.Vector (T.Text, T.Text) -> Question
createQuestion url io = Question { qurl = url, qio = io }

createAtSubmit :: [T.Text] -> T.Text -> AtSubmit
createAtSubmit r u = (listAtSubmit r) { rcom = T.unwords r
                                      , userdir = u}
 where
  listAtSubmit :: [T.Text] -> AtSubmit
  listAtSubmit l = case Prelude.length l of
   1 -> nullAtSubmit {subcmd = l !! 0, cname = Nothing, qname = Nothing, file = Nothing} 
   2 -> nullAtSubmit {subcmd = l !! 0, cname = Just (T.takeWhile (/= '_') (l !! 1)), qname = Just (l !! 1), file = Nothing}
   3 -> nullAtSubmit {subcmd = l !! 0, cname = Just (T.takeWhile (/= '_') (l !! 1)), qname = Just (l !! 1), file = Just (l !! 2)}
   _ -> nullAtSubmit

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


getCookieAndCsrfToken :: T.Text -> T.Text -> IO Contest
getCookieAndCsrfToken un pw = do
 fstreq <- parseRequest "https://atcoder.jp/login"
 fstmng <- newManager tlsManagerSettings
 fstres <- Network.HTTP.Conduit.httpLbs fstreq fstmng
 let csrf_tkn = (getCsrfToken.decodeUtf8.BSL.toStrict.getResponseBody) fstres
 let fstcookie = getResponseHeader hSetCookie fstres
 responce <- do
  req <- setRequestHeader hCookie fstcookie.setRequestMethod "POST" <$> parseRequest "https://atcoder.jp/login"
  let postReq = urlEncodedBody [("username", encodeUtf8 un),
                                ("password", encodeUtf8 pw),
                                ("csrf_token",encodeUtf8 csrf_tkn)] req
  manager <- newManager tlsManagerSettings
  Network.HTTP.Conduit.httpLbs postReq manager
 return $ if getResponseStatus responce /= status200 then nullContest 
          else createContest V.empty (getResponseHeader hSetCookie responce) csrf_tkn
 where
  getCsrfToken :: T.Text -> T.Text
  getCsrfToken body = T.takeWhile (/= '\"') $ snd $ T.breakOnEnd (T.pack "value=\"") body

getPageInfo :: AtSubmit -> Contest -> IO Question
getPageInfo msg ud = case ((cname msg, qname msg)) of
 (Just cm, Just qm) -> do
  let questurl = T.append (T.pack "https://atcoder.jp/contests/") $ T.append cm $ T.append (T.pack "/tasks/") qm
  r <- parseRequest $ T.unpack questurl
  let req = setRequestHeader hCookie (cookie ud) r
  mng <- newManager tlsManagerSettings
  res <- Network.HTTP.Conduit.httpLbs req mng
  if getResponseStatus res /= status200 then return nullQuestion 
  else return $ createQuestion questurl ((questionIO.fromDocument.parseLBS.getResponseBody) res)
 _ -> return nullQuestion
 where 
  questionIO :: Cursor -> V.Vector (T.Text, T.Text)
  questionIO cursor = do
   let cs = Prelude.map child $ cursor $// attributeIs "class" "col-sm-12" &// element "section" &// element "pre" 
   V.fromList.ioZip $ Prelude.map ((`T.append` "\n"). chnl).concatMap content $ Prelude.concat.Prelude.tail $ cs
  ioZip :: [T.Text] -> [(T.Text, T.Text)]
  ioZip (i:o:lists) 
   | T.null i || T.singleton '\n' == i ||  T.null o || T.singleton '\n' == o  = []
   | Prelude.null lists || (T.null.Prelude.head) lists                        = [(i, o)] 
   | otherwise                                                                = (i, o):ioZip lists
  chnl :: T.Text -> T.Text
  chnl = T.dropWhile (\x -> (x==' ')||(x=='\n')).T.dropWhileEnd (\x -> (x==' ')||(x=='\n')).T.replace (T.pack "\r\n") (T.pack "\n")

postSubmit :: AtSubmit -> Contest -> IO ()
postSubmit msg ud = case ((cname msg, qname msg, file msg)) of
 (Just cm, Just qm, Just fm) -> do
  let questurl = T.append (T.pack "https://atcoder.jp/contests/") $ T.append cm (T.pack "/submit")
  source <- TIO.readFile $ T.unpack $ T.append (userdir msg) $ T.append (T.singleton '/') fm
  r <- parseRequest $ T.unpack questurl
  let req = setRequestHeader hCookie (cookie ud) r
  response <- do
   let postReq = urlEncodedBody [ ("data.TaskScreenName",encodeUtf8 qm)
                                , ("data.LanguageId", (encodeUtf8.T.pack) "3014") -- this id is only Haskell
                                , ("sourceCode", encodeUtf8 source)
                                , ("csrf_token", (encodeUtf8.csrf_token) ud)] req
   manager <- newManager tlsManagerSettings
   Network.HTTP.Conduit.httpLbs postReq manager
  return ()
 _ -> return ()
 
postLogout :: Contest -> IO()
postLogout ud = do
 let url = "https://atcoder.jp/logout"
 postReq <- urlEncodedBody [("csrf_token", (encodeUtf8.csrf_token) ud)] <$> parseRequest url
 manager <- newManager tlsManagerSettings
 Network.HTTP.Conduit.httpLbs postReq manager
 return ()

testLoop :: V.Vector (T.Text, T.Text) -> System.IO.FilePath -> Int -> IO T.Text
testLoop qs dir k = if V.null qs then return T.empty else do
 TIO.writeFile infile $ (fst.V.head) qs
 TIO.writeFile outfile $ (snd.V.head) qs
 ec <- shell dockershell empty
 outres <- TIO.readFile outfile
 comp <- TIO.readFile compfile
 let out = case ec of
                ExitFailure 1 -> T.append "CE\n" comp
                ExitFailure 2 -> "WA (input error)\n"
                ExitFailure _ -> "TLE\n"
                ExitSuccess   -> if outres == (snd.V.head) qs then "AC" 
                                 else T.append "WA\n" $ T.append "=== result ===\n" $ T.append outres 
                                                      $ T.append "=== sample ===\n" $ (snd.V.head) qs
 next <- testLoop (V.tail qs) dir (k+1)
 return $ T.append (T.append (msgCreate k) out) next
  where
   msgCreate :: Int -> T.Text
   msgCreate n = T.append "case " $ T.append ((T.pack.show) n) ": "
   infile = dir ++ "/.cache/atsubmit/src/input.txt"
   outfile = dir ++ "/.cache/atsubmit/src/outres.txt"
   compfile = dir ++ "/.cache/atsubmit/src/comp.txt"
