{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Server where

import Lib
import Types

import Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import qualified Data.Aeson as DA
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L
import qualified Data.Map as M
import Text.HTML.DOM
import qualified Text.XML as TX
import Text.XML.Cursor
import Control.Monad
import Control.Applicative
import Control.Exception
import qualified Control.Foldl as CF
import qualified Control.Exception as E
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import System.Directory
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.Socket
import Network.HTTP.Types.Header
import qualified Network.URI.Encode as NUE

runServer :: FilePath -> (Socket -> IO Bool) -> IO ()
runServer path server = do
 result <- withSocketsDo $ E.bracket (open path) close doSocket
 when result $ runServer path server
 where
  doSocket :: Socket -> IO Bool
  doSocket s = do
   (conn, peer) <- accept s
   result <- server conn
   close conn
   return result
  open :: FilePath -> IO Socket
  open path = do
   sock <- socket AF_UNIX Stream 0
   rmFile path
   ready sock
  ready s = do  
   bind s (SockAddrUnix path)
   listen s 1
   return s -- ready

server :: (Socket -> SHelperServerRequest -> IO SHelperServerResponce) -> Socket -> IO Bool
server action sock = do
        raw <- fromStrict <$> recvMsg sock 1024
        Prelude.print raw
        case DA.decode raw of
                Just request -> do
                        response <- action sock request
                        Prelude.print request
                        Prelude.print response
                        Prelude.print ((toStrict.DA.encode) response)
                        sendMsg sock ((toStrict.DA.encode) response) 1024
                        return $ response /= SHelperOk (StopRes ())
                Nothing -> do 
                        sendMsg sock ((toStrict.DA.encode) JsonParseError) 1024
                        return True

actionSHelper :: MVar Contest -> Socket -> SHelperServerRequest -> IO SHelperServerResponce
actionSHelper contest sock (SHelperServerRequest request) = do
 ret <- requestHandler `catch` \(e :: SHelperException) -> return (SHelperErr e)
                       `catch` \(e :: SomeException) -> return (SHelperErr Unknown)
 let req = case request of LoginReq username password -> LoginReq (Username "") (Password "")
                           _                          -> request
 cnt <- readMVar contest
 swapMVar contest $ cnt {rlogs = V.snoc (rlogs cnt) (RLog (SHelperServerRequest req, ret))}
 return ret 
 where
  requestHandler :: IO SHelperServerResponce
  requestHandler = requestCheck request >>= \case 
   Err msg -> return $ SHelperErr (BadData msg)
   Ok -> case request of 
        LoginReq username password -> do
                result <- evalSHelper contest (Login username password) 
                return $ SHelperOk (LoginRes result)
        QGetReq qname ud -> do
                result <- evalSHelper contest (QGet qname ud)
                return $ SHelperOk (QGetRes result)
        CGetReq cname ud -> do
                result <- evalSHelper contest (CGet cname ud)
                return $ SHelperOk (CGetRes result)
        TestReq source qname -> do
                result <- evalSHelper contest (Test sock source qname)
                return $ SHelperOk (TestRes result)
        SubmitReq source qname -> do
                result <- evalSHelper contest (Submit source qname)
                return $ SHelperOk (SubmitRes result)
        DebugReq source debug_input -> do
                result <- evalSHelper contest (Types.Debug source debug_input)
                return $ SHelperOk (DebugRes result)
        PrintReq -> do
                result <- evalSHelper contest Print
                return $ SHelperOk (PrintRes result)
        ShowReq qname -> do
                result <- evalSHelper contest (Show qname)
                return $ SHelperOk (ShowRes result)
        ResultReq cname -> do
                result <- evalSHelper contest (Result cname)
                return $ SHelperOk (ResultRes result)
        LogReq -> do
                result <- evalSHelper contest Log
                return $ SHelperOk (LogRes result)
        StopReq -> do
                result <- evalSHelper contest Stop
                return $ SHelperOk (StopRes result)
        LogoutReq -> do
                result <- evalSHelper contest Logout
                return $ SHelperOk (LogoutRes result)

requestCheck :: SHelperRequest -> IO CheckErr
requestCheck (LoginReq (Username "") _) = return $ Err "don't set username."
requestCheck (LoginReq _ (Password "")) = return $ Err "don't set password."
requestCheck (LoginReq (Username user) (Password pass)) = return Ok
requestCheck (QGetReq [] _) = return $ Err "don't set question name." 
requestCheck (QGetReq _ (Userdir "")) = return $ Err "don't set user working directory."
requestCheck (QGetReq qname userdir) = return Ok
requestCheck (CGetReq [] _) = return $ Err "don't set contest name." 
requestCheck (CGetReq _ (Userdir "")) = return $ Err "don't set user working directory."
requestCheck (CGetReq cname userdir) = return  Ok
requestCheck (TestReq (Source "") _) = return $ Err "don't set source file."
requestCheck (TestReq _ (QName "")) =  return $ Err "don't set question name."
requestCheck (TestReq (Source source) qname) = doesFileExist source >>= \x -> return $ if x then Ok else Err $ fileNotExists source
requestCheck (SubmitReq (Source "") _) = return $ Err "don't set source file."
requestCheck (SubmitReq _ (QName "")) = return $ Err "don't set question name."
requestCheck (SubmitReq (Source source) qname) = doesFileExist source >>= \x -> return $ if x then Ok else Err $ fileNotExists source
requestCheck (DebugReq (Source "") _) = return $ Err "don't set source file."
requestCheck (DebugReq _ (DIn "")) = return $ Err "don't set debug file."
requestCheck (DebugReq (Source source) (DIn din)) = doesFileExist source >>= \x -> doesFileExist din >>= \y -> 
        return $ if x then if y then Ok else Err (fileNotExists din) else Err $ fileNotExists source
requestCheck PrintReq = return Ok
requestCheck (ShowReq (QName "")) =  return $ Err "don't set question name."
requestCheck (ShowReq qname) = return Ok
requestCheck (ResultReq (CName "")) = return $ Err "don't set contest name."
requestCheck (ResultReq cname) = return Ok
requestCheck LogReq = return Ok
requestCheck StopReq = return Ok
requestCheck LogoutReq = return Ok

fileNotExists :: FilePath -> T.Text
fileNotExists fn = V.foldl1 T.append ["file \"", T.pack fn ,"\" doesn't exist."]

evalSHelper :: MVar Contest -> SHelper a -> IO a

evalSHelper mvcont (Login (Username user) (Password pass)) = do
 contest <- readMVar mvcont
 fstres <- getRequestWrapper "https://atcoder.jp/login" []
 let csrf_tkn = scrapingCsrfToken fstres
 let fstcke = getResponseHeader hSetCookie fstres
 response <- postRequestWrapper "https://atcoder.jp/login" fstcke [ ("username", user), ("password", pass), ("csrf_token", csrf_tkn)]
 when ((checkFailLogin.getResponseBody) response) $ createContest V.empty [] [] >>= \x -> swapMVar mvcont x >> throwIO FailLogin

 createContest V.empty (getResponseHeader hSetCookie response) csrf_tkn >>= \x -> swapMVar mvcont x
 return ()
 where
  checkFailLogin :: BSL.ByteString -> Bool
  checkFailLogin = Prelude.null.($// attributeIs "class" "alert alert-success alert-dismissible col-sm-12 fade in").fromDocument.parseLBS
  scrapingCsrfToken :: Response ByteString -> T.Text
  scrapingCsrfToken = (M.! TX.Name {TX.nameLocalName = "value", TX.nameNamespace = Nothing, TX.namePrefix = Nothing}).
                       TX.elementAttributes.(\case TX.NodeElement a -> a).Prelude.head.Prelude.map node.
                       ($// attributeIs "name" "csrf_token").fromDocument.parseLBS.getResponseBody

evalSHelper mvcont (QGet qn (Userdir ud)) = do
 contest <- readMVar mvcont
 (ret, next) <- V.unzip <$> mapM (\x -> getPageInfo x ud contest) qn `catch` \(e :: SHelperException) -> throwIO e 
                                                                     `catch` \(e :: SomeException) -> throwIO e
 swapMVar mvcont $ contest {questions = questions contest V.++ V.filter (/=nullQuestion) next}
 return ret

evalSHelper mvcont (CGet cn (Userdir ud)) = do
 contest <- readMVar mvcont
 (res, quests) <- V.unzip <$> mapM (\x -> getContestInfo x ud contest) cn `catch` \(e :: SHelperException) -> throwIO e
                                                                          `catch` \(e :: SomeException) -> throwIO e
 swapMVar mvcont $ contest {questions = questions contest V.++ (V.filter (/=nullQuestion).V.concat.V.toList) quests}
 return $ (V.concat.V.toList) res

evalSHelper mvcont (Test sock (Source source) (QName qn)) = do
 contest <- readMVar mvcont
 lang <- languageSelect (homedir contest) (T.pack source)
 let func = if is_docker lang then useDockerTest (docker_image lang) contest
                              else unUseDocker (compile lang) (exec lang) contest
 copyFile source ((T.unpack.main_file) contest)
 let mquest = V.find ((== qn).T.takeWhileEnd (/='/').qurl) $ questions contest
 case mquest of Nothing -> throwIO $ NotGetQuestion (QName qn) -- not getting
                Just a  -> testLoop contest (qio a) func (T.append "Main." ((V.head.extention) lang))
                            `catch` \(e :: SHelperException) -> throwIO e
 where
  testLoop :: Contest -> V.Vector (T.Text, T.Text) -> (T.Text -> IO (Maybe Int)) -> T.Text -> IO ()
  testLoop c qs func main = if V.null qs then return () else do
   TIO.writeFile ((T.unpack.input_file) c) $ (fst.V.head) qs
   ec <- func main
   outres <- TIO.readFile ((T.unpack.output_file) c)
   comp <- TIO.readFile ((T.unpack.compile_file) c)
   let res = case ec of
                    Just 0  -> if checkResult (T.lines outres) ((T.lines.snd.V.head) qs)
                             then AC
                             else WA (TOut outres) (TAns ((snd.V.head) qs))
                    Just 1  -> CE (Message comp)
                    Just 2  -> RE
                    Just _  -> TLE
                    Nothing -> IE
   sendMsg sock ((toStrict.DA.encode) res) 1024
   case res of CE comp -> return ()
               IE      -> throwIO InternalError
               _       -> testLoop c (V.tail qs) func main

evalSHelper mvcont (Submit (Source source) (QName qn)) = do
 contest <- readMVar mvcont
 src <- TIO.readFile source
 lang <- languageSelect (homedir contest) (T.pack source)
 let cn = T.takeWhile (/='_') qn
     questurl = V.foldl1 T.append ["https://atcoder.jp/contests/", cn, "/submit"]
 res <- postRequestWrapper questurl (cookie contest) [ ("data.TaskScreenName", qn), ("data.LanguageId", langid lang)
                                                     , ("sourceCode", src), ("csrf_token", csrf_token contest)]
 return ()

evalSHelper mvcont (Types.Debug (Source source) (DIn din)) = do
 contest <- readMVar mvcont
 lang <- languageSelect (homedir contest) (T.pack source)
 copyFile source ((T.unpack.main_file) contest)
 copyFile din ((T.unpack.input_file) contest) 
 ec <- if is_docker lang then useDockerTest (docker_image lang) contest (T.append "Main." ((V.head.extention) lang))
                         else unUseDocker (compile lang) (exec lang) contest (T.append "Main." ((V.head.extention) lang))
 outres <- TIO.readFile ((T.unpack.output_file) contest)
 comp <- TIO.readFile ((T.unpack.compile_file) contest) 
 dinp <- TIO.readFile ((T.unpack.input_file) contest)
 return $ case ec of Just 0  -> DAC (DOut outres)
                     Just 1  -> DCE (Message comp)
                     Just 2  -> DRE
                     Just _  -> DTLE
                     Nothing -> DIE

evalSHelper mvcont Print = do
 contest <- readMVar mvcont 
 return $ V.map (QName . T.takeWhileEnd (/='/').qurl) (questions contest)

evalSHelper mvcont (Show (QName qn)) = do
 contest <- readMVar mvcont
 let mquest = V.find ((== qn).T.takeWhileEnd (/='/').qurl) $ questions contest
 case mquest of Nothing -> throwIO $ NotGetQuestion (QName qn)
                Just a  -> do
                           let qios = qio a
                           return $ QIO qios

evalSHelper mvcont (Result (CName cn)) = do
 contest <- readMVar mvcont
 res <- getRequestWrapper (V.foldl1 T.append ["https://atcoder.jp/contests/", cn, "/submissions/me"]) (cookie contest)
 when (getResponseStatus res /= status200) $ throwIO Unknown
 return $ CResult $ result.fromDocument.parseLBS.getResponseBody $ res
  where
   result :: Cursor -> [[T.Text]]
   result cursor = do
    let subtime = Prelude.concatMap content.Prelude.concatMap child $ cursor $// attributeIs "class" "table-responsive"
                                                                             &// attributeIs "class" "fixtime fixtime-second"
        c = Prelude.concatMap content.lineNGet (cOrP cursor).Prelude.concatMap child $ cursor $// attributeIs "class" "table-responsive"
                                                                                              &// element "td"
                                                                                              &// element "a" -- [question, uname, details]
        result = Prelude.concatMap content.Prelude.concatMap child $ cursor $// attributeIs "class" "table-responsive"
                                                                            &// element "td"
                                                                            &// attributeIs "aria-hidden" "true"
    zipLines subtime c result
   cOrP :: Cursor -> Int 
   cOrP cs = do
    let ch = cs $// attributeIs "class" "table-responsive"
                &// element "td"
                &// attributeIs "class" "glyphicon glyphicon-search black"
    if Prelude.null ch then 3 else 4
   lineNGet :: Int -> [Cursor] -> [Cursor]
   -- in contest, lineNGet 3 list else lineNGet 4
   lineNGet k l = if Prelude.length l >= k then Prelude.head l:lineNGet k (Prelude.drop k l) else [] 
   zipLines :: [T.Text] -> [T.Text] -> [T.Text] -> [[T.Text]]
   zipLines [] [] [] = [] 
   zipLines [s] [c] [r] = [[s, c, r]]
   zipLines (s:t) (c:n) (r:e) = [s, c, r]:zipLines t n e

evalSHelper mvcont Stop = do
 contest <- readMVar mvcont
 BSC.writeFile ((T.unpack.homedir) contest ++ cookieFile) ((BSC.unlines.cookie) contest)
 return ()

evalSHelper mvcont Log = do
 contest <- readMVar mvcont
 return $ rlogs contest

evalSHelper mvcont Logout = do
 contest <- readMVar mvcont
 res <- postRequestWrapper "https://atcoder.jp/logout" (cookie contest) [("csrf_token", csrf_token contest)]
 when (getResponseStatus res /= status200) $ throwIO Unknown
 rmFile $ T.unpack (homedir contest) ++ cookieFile
 swapMVar mvcont $ contest { cookie = [], csrf_token = "" }
 return ()

getContestInfo :: CName -> FilePath -> Contest -> IO (V.Vector GetResult, V.Vector Question) -- (question names)
getContestInfo (CName cn) userdir contest = getRequestWrapper contesturl (cookie contest) >>= \res ->
  if getResponseStatus res /= status200 then return (V.singleton (ContestNotExist (CName cn)),V.singleton nullQuestion)
  else mapM (\x -> getPageInfo (QName x) userdir contest) ((V.fromList.quests.fromDocument.parseLBS.getResponseBody) res) >>= \x -> 
   return $ V.unzip x
 where
  contesturl = V.foldl1 T.append ["https://atcoder.jp/contests/", cn, "/tasks"]
  quests :: Cursor -> [T.Text]
  quests = Prelude.map (T.takeWhileEnd (/='/')).Prelude.concatMap (attribute "href").
           ($// attributeIs "class" "text-center no-break" &// element "a")

getPageInfo :: QName -> FilePath -> Contest -> IO (GetResult, Question)
getPageInfo (QName qn) userdir contest = 
-- if V.elem questurl ((V.map qurl.questions) contest) then return (AlreadyGet (QName qn), nullQuestion)
 if V.elem qn (V.map (T.takeWhileEnd (/='/').qurl) (questions contest)) then return (AlreadyGet (QName qn),nullQuestion)
 else doesFileExist fname >>= \fcheck -> 
  if fcheck then Text.HTML.DOM.readFile fname >>= \res -> return (FromLocal (QName qn), newQuest res)
  else getRequestWrapper questurl (cookie contest) >>= \res ->
   if getResponseStatus res /= status200 then return (QuestionNotExist (QName qn), nullQuestion)
   else do
    TIO.writeFile fname ((rewriteHtml.decodeUtf8.BSL.toStrict.getResponseBody) res)
    let quest = createQuestion questurl ((questionIO.fromDocument.parseLBS.getResponseBody) res)
    return (GetResultOk (QName qn), (newQuest.parseLBS.getResponseBody) res)
 where
  questurl = V.foldl1 T.append ["https://atcoder.jp/contests/", T.takeWhile (/='_') qn, "/tasks/", qn]
  fname = T.unpack (V.foldl1 T.append [T.pack userdir, "/", qn, ".html"])
  newQuest raw = createQuestion questurl ((questionIO.fromDocument) raw)

questionIO :: Cursor -> V.Vector (T.Text, T.Text)
questionIO cursor = do
 let cs = Prelude.map child $ cursor $// attributeIs "class" "col-sm-12" &// element "section" &// element "pre" 
 V.fromList.ioZip $ Prelude.map chnl.Prelude.concatMap content $ Prelude.concat.Prelude.tail $ cs

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

cookieCsrfToken :: BSC.ByteString -> T.Text
cookieCsrfToken = decodeUtf8.(\case
 Nothing -> ""
 Just a -> BSC.drop 11 a).L.find (\x -> BSC.pack "csrf_token"== BSC.take 10 x).BSC.split '\NUL'.NUE.decodeByteString
-- getCsrfToken :: T.Text -> T.Text
-- getCsrfToken = T.replace "&#43;" "+".T.takeWhile (/= '\"').snd.T.breakOnEnd (T.pack "value=\"") 
