{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lib where

import Types

import Data.ByteString.Lazy
import Network.Socket
import qualified Data.List as L
import qualified Data.ByteString as S
import qualified Data.Text as T
import Data.Text.Encoding
import System.IO
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Network.HTTP.Conduit
import qualified Network.Socket.ByteString as NSBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Bifunctor
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath
import qualified Data.Aeson as DA
import Turtle
import Turtle.Line
import qualified Control.Foldl as CF
import Control.Concurrent.Timeout
import Control.Exception
import Text.XML.Cursor
import Text.XML

-- filepath setting
langJson = normalise ".config/atsubmit/lang_conf.json"
helpFile = normalise ".local/share/man/atsubmit.man"
cookieFile = normalise ".cache/atsubmit/cookie"
sockpath = normalise ".local/lib/atsubmit/atsubmit.sock"

nullContest = Contest { questions = [], csrf_token = "",rlogs = [], homedir = ""
                      , input_file = "", compile_file = "", output_file = ""}

nullQuestion = Question { qurl = "", qsentence = "", qrestriction = [], qio = ("", ""), qiosample = []}

nullLangJson = LangJson { name = "", Types.extension = [], is_docker = False, docker_image = Nothing, docker_comp = Nothing, docker_exec = Nothing
                        , compile = Nothing, exec = Nothing, langid = ""}

createContest :: V.Vector Question -> T.Text -> IO Contest
createContest q t = getHomeDirectory >>= \d ->
                      return Contest { questions = q, csrf_token = t, rlogs = V.empty, homedir = d
                                     , input_file = d System.FilePath.</> normalise ".cache/atsubmit/src/input.txt"{- in docker /home/input.txt -}
                                     , compile_file = d System.FilePath.</> normalise ".cache/atsubmit/src/comp.txt"{- in docker /home/comp.txt -}
                                     , output_file = d System.FilePath.</> normalise ".cache/atsubmit/src/outres.txt"}{- in docker /home/outres.txt -}

createQuestion :: T.Text -> T.Text -> V.Vector T.Text -> (T.Text, T.Text) -> V.Vector (T.Text, T.Text) -> Question
createQuestion url sentence restriction io iosample = Question { qurl = url, qsentence = sentence, qrestriction = restriction, qio = io, qiosample = iosample}

getAtKeys :: IO [String]
getAtKeys = do
 hSetEcho System.IO.stdin False
 System.IO.putStrLn "============== atcoder username and password ==============="
 apis <- getAPIkeys ["username : ", "password : "]
 hSetEcho System.IO.stdin True
 return apis

getAPIkeys :: [String] -> IO [String]
getAPIkeys [] = return []
getAPIkeys (m:messages) = do
 Prelude.putStr m 
 hFlush System.IO.stdout
 api <- Prelude.getLine 
 Prelude.putChar '\n'
 getAPIkeys messages >>= (\res -> return (api:res))

recvMsg :: Socket -> Int -> IO S.ByteString
recvMsg sock n = do 
 json <- fromStrict <$> NSBS.recv sock n 
 case DA.decode json of
  Just size -> do
   NSBS.send sock $ toStrict.DA.encode $ size {socksize = min (socksize size) n} -- deside receive size
   rcv <- recvLoop (min (socksize size) n) 0
   return $ (S.take (datasize size).Prelude.foldl1 S.append) rcv
    where
     recvLoop :: Int -> Int -> IO [S.ByteString]
     recvLoop k i = do
      msg <- NSBS.recv sock k
      if S.length msg + i >= datasize size then NSBS.send sock (toStrict "end") >> return [msg]
      else recvLoop k (S.length msg + i) >>= \next -> return (msg:next)
  _         -> return S.empty
 `catch` (\(SomeException e) -> throwIO e)

sendMsg :: Socket -> S.ByteString -> Int -> IO BS.ByteString
sendMsg sock msg n = do
 NSBS.send sock $ toStrict.DA.encode $ Sizes {socksize = n, datasize = S.length msg}
 raw <- timeout 1000 (NSBS.recv sock n) -- deside send size
 case raw of Just json -> case (DA.decode.fromStrict) json of Just size -> sendLoop (takeNList (socksize size) msg)
                                                              _         -> return BS.empty
             Nothing   -> sendMsg sock msg n -- resend timeout
 `catch` (\(SomeException e) -> throwIO e)
 where
  sendLoop :: [S.ByteString] -> IO BS.ByteString
  sendLoop m = do 
   NSBS.send sock $ Prelude.head m
   if Prelude.length m == 1 then NSBS.recv sock n
   else sendLoop (Prelude.tail m)

rmDup :: V.Vector T.Text -> V.Vector T.Text
rmDup = V.foldl (\seen x -> if V.elem x seen then seen else V.cons x seen) V.empty

takeNList :: Int -> BS.ByteString -> [BS.ByteString]
takeNList n base = BS.take n base:(if BS.length base < n then [] else takeNList n (BS.drop n base))

getRequestWrapper :: T.Text -> String -> IO (Response BSL.ByteString)
getRequestWrapper url homedir = do
 fexist <- doesFileExist (homedir System.FilePath.</> cookieFile)
 cke <- if fexist then BSC.readFile (homedir System.FilePath.</> cookieFile) else return BS.empty
 req <- if Prelude.null (BSC.lines cke) then parseRequest (T.unpack url)
        else setRequestHeader hCookie (BSC.lines cke) <$> parseRequest (T.unpack url)
 mng <- newManager tlsManagerSettings
 response <- Network.HTTP.Conduit.httpLbs req mng
 BSC.writeFile (homedir System.FilePath.</> cookieFile) (BSC.unlines (getResponseHeader hSetCookie response))
 return response

postRequestWrapper :: T.Text -> String -> [(BSC.ByteString, T.Text)] -> IO (Response BSL.ByteString)
postRequestWrapper url homedir body = do
 fexist <- doesFileExist (homedir System.FilePath.</> cookieFile)
 cke <- if fexist then BSC.readFile (homedir System.FilePath.</> cookieFile) else return BS.empty
 req <- setRequestHeader hCookie (BSC.lines cke) <$> parseRequest (T.unpack url)
 let postReq = urlEncodedBody (Prelude.map (second encodeUtf8) body) req
 mng <- newManager tlsManagerSettings
 response <- Network.HTTP.Conduit.httpLbs postReq mng
 BSC.writeFile (homedir System.FilePath.</> cookieFile) (BSC.unlines (getResponseHeader hSetCookie response))
 return response

-- name, extention, docker_image, langid
languageSelect :: System.FilePath.FilePath -> System.FilePath.FilePath -> IO LangJson
languageSelect home fp = do
 json <- BSL.fromStrict <$> (BS.readFile.combine home) langJson
 case DA.decode json :: Maybe LJBase of
  Nothing -> return nullLangJson
  Just lists -> case V.find (V.elem ((T.pack.L.tail.takeExtension) fp).Types.extension) (language lists) of 
                     Nothing   -> return nullLangJson
                     Just lang -> return lang
 where
  getExtention :: System.FilePath.FilePath -> T.Text
  getExtention = T.takeWhileEnd (/= '.').T.pack

checkResult :: [T.Text] -> [T.Text] -> Bool
checkResult [] []           = True
checkResult ([]:es) ans   = checkResult es ans
checkResult (r:es) (a:ns)   = r == a && checkResult es ns
checkResult _ _             = False

compileWithContainer :: Contest -> Source -> LangJson -> IO T.Text
compileWithContainer contest (Source (wd, src)) lj = case (docker_image lj, docker_comp lj) of
 (Just dimage, Just compcmd) -> do 
  shell (V.foldl1 T.append ["docker run --name atsubmit_run --pids-limit 100 --network none  -dit ", dimage, " sh"]) Turtle.empty
  shell (V.foldl1 T.append ["docker cp ", T.pack (wd System.FilePath.</> src), " atsubmit_run:/home/Main", (T.pack.takeExtension) src]) Turtle.empty
  ec <- shell (V.foldl1 T.append ["docker exec atsubmit_run bash -c \"", compcmd, " > /home/comp.txt 2>&1\""]) Turtle.empty
  shell (V.foldl1 T.append ["docker cp atsubmit_run:/home/comp.txt ", (T.pack.compile_file) contest]) Turtle.empty
  case ec of
   ExitSuccess -> return ""
   _           -> rdFile $ compile_file contest
 (Nothing, Just _) -> throwIO InternalError
 (_, Nothing) -> throwIO InternalError
 where 
  rdFile :: System.FilePath.FilePath -> IO T.Text
  rdFile path = doesFileExist path >>= \x -> if x then TIO.readFile path else return $ V.foldl1 T.append ["File ", T.pack path, " Not Exists"]

execWithContainer :: Contest -> Source -> LangJson -> IO (Maybe Int)
execWithContainer contest (Source (wd, src)) lj = case docker_exec lj of
 Just execmd -> do
  shell (V.foldl1 T.append ["docker cp ", (T.pack.input_file) contest, " atsubmit_run:/home/input.txt"]) Turtle.empty
  ec <- shell (V.foldl1 T.append ["timeout 2 docker exec atsubmit_run bash -c \"", execmd, " < /home/input.txt > /home/output.txt \"" ]) Turtle.empty
  timecheck <- Turtle.fold (inshell "docker inspect atsubmit_run --format=\'{{.State.ExitCode}}\'" Turtle.empty) CF.head
  shell (V.foldl1 T.append ["docker cp atsubmit_run:/home/output.txt ", (T.pack.output_file) contest]) Turtle.empty
  case (ec,timecheck) of
   (ExitSuccess, Just "0") -> return $ Just 0
   (ExitSuccess, Just m)   -> return $ Just $ (read.T.unpack.lineToText) m
   (ExitFailure 124, Just m) -> return $ Just 124
   (ExitFailure n, Just m) -> return $ Just $ if n == (read.T.unpack.lineToText) m then n else (read.T.unpack.lineToText) m 

stopContainer :: IO ()
stopContainer = void $ shell "docker stop atsubmit_run; docker rm -f atsubmit_run" Turtle.empty

compileWithoutContainer :: Contest -> Source -> LangJson -> IO T.Text
compileWithoutContainer contest (Source (wd, src)) lj = case compile lj of
 Nothing -> throwIO InternalError
 Just compcmd -> do
  comp <- shell (V.foldl1 T.append [compcmd, " > ", (T.pack.compile_file) contest, " 2>&1"]) Turtle.empty
  case comp of
   ExitFailure _ -> rdFile $ compile_file contest
   ExitSuccess   -> return ""
 where
  rdFile :: System.FilePath.FilePath -> IO T.Text
  rdFile path = doesFileExist path >>= \x -> if x then TIO.readFile path else return $ V.foldl1 T.append ["File ", T.pack path, " Not Exists"]

execWithoutContainer :: Contest -> Source -> LangJson -> IO(Maybe Int)
execWithoutContainer contest (Source (wd, src)) lj = case exec lj of 
 Nothing -> throwIO InternalError
 Just execmd -> do
  exe <- shell (V.foldl1 T.append ["timeout 2 sh -c \"", execmd, " < ", (T.pack.input_file) contest, " > ", (T.pack.output_file) contest, "\""]) Turtle.empty
  case exe of
   ExitFailure 124 -> return $ Just 124
   ExitFailure n   -> return $ Just 2
   ExitSuccess     -> return $ Just 0

stopWithoutContainer :: IO()
stopWithoutContainer = return ()

rmFile :: System.IO.FilePath -> IO()
rmFile path = doesFileExist path >>= \x -> when x (removeFile path)

scrapeNodes curs = case child curs of []    -> content curs
                                      next  -> L.concatMap scrapeNodes next

-- scrapeNodesWithLaTeX
snwl curs = case node curs of NodeElement e -> case (nameLocalName.elementName) e of "var" -> L.concatMap snocons (child curs)
                                                                                     "li"  -> (" * ":).L.concatMap (snwl.fromNode) $ elementNodes e
                                                                                     "pre" -> ("```\n":).L.foldr (:) ["```"] $ L.concatMap (snwl.fromNode) $ elementNodes e
                                                                                     _     -> L.concatMap (snwl.fromNode) (elementNodes e)
                              _             -> content curs
 where
  snocons = L.map ((`T.snoc` '$').T.cons '$').content
