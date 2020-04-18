{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lib where

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
import qualified Network.URI.Encode as NUE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import qualified Data.Aeson as DA
import Turtle
import Turtle.Line
import qualified Control.Foldl as CF

data  Sizes = Sizes { socksize :: Int
                    , datasize :: Int
                    } deriving (Show, Eq)

instance DA.ToJSON Sizes where
 toJSON (Sizes s d) = DA.object [ "socksize" DA..= s, "datasize" DA..= d]

instance DA.FromJSON Sizes where
 parseJSON (DA.Object v) = Sizes <$> (v DA..: "socksize") <*> (v DA..: "datasize" )

langJson = "/.config/atsubmit/lang_conf.json"
helpFile = "/.local/share/man/atsubmit.man"
cookieFile = "/.cache/atsubmit/cookie"

type AtFunc = Contest -> ReqAtSubmit -> IO (Either (Int, T.Text) (Contest, ResAtSubmit))

data Question = Question { qurl :: T.Text -- question page's url
                         , qio :: V.Vector (T.Text, T.Text) -- input, output
                         } deriving (Show, Eq)

data Contest = Contest { questions :: V.Vector Question 
                       , cookie :: [BSC.ByteString]
                       , csrf_token :: T.Text
                       , homedir :: T.Text
                       , main_file :: T.Text
                       , input_file :: T.Text
                       , compile_file :: T.Text
                       , output_file :: T.Text
                       } deriving (Show, Eq)

data ReqAtSubmit = ReqAtSubmit { rcom :: T.Text -- raw command
                               , subcmd :: T.Text -- text
                               , cname :: Maybe T.Text -- contest name (ex abc120 
                               , qname :: Maybe T.Text -- question name (ex abc_a
                               , file :: Maybe T.Text -- have to full path
                               , userdir :: T.Text
                               , password :: Maybe T.Text -- only use login
                               , username :: Maybe T.Text -- only use login
                               , debug_input :: T.Text -- only use debug
                               } deriving (Show, Eq)

data ResAtSubmit = ResAtSubmit { resstatus :: Int    -- responce status 
                               , resmsg    :: T.Text -- responce message
                               , resresult :: [[T.Text]] -- [WA, output, testcase] or [input, output]
                               } deriving (Show, Eq)

data LangJson = LangJson { name :: T.Text
                         , extention :: V.Vector T.Text
                         , is_docker :: Bool
                         , docker_image :: Maybe T.Text
                         , compile :: Maybe T.Text
                         , exec :: Maybe T.Text
                         , langid :: T.Text
                         } deriving (Show, Eq)

data LJBase = LJBase { language :: V.Vector LangJson } deriving (Show, Eq)

instance DA.FromJSON ReqAtSubmit where
 parseJSON (DA.Object v) = ReqAtSubmit <$> (v DA..: "rcom")
                                       <*> (v DA..: "subcmd")
                                       <*> (v DA..:? "cname")
                                       <*> (v DA..:? "qname")
                                       <*> (v DA..:? "file")
                                       <*> (v DA..: "userdir")
                                       <*> (v DA..:? "username")
                                       <*> (v DA..:? "password")
                                       <*> (v DA..: "debug_input")

instance DA.ToJSON ReqAtSubmit where
 toJSON (ReqAtSubmit rc sc cn qn f u un ps di) = DA.object [ "rcom" DA..= rc
                                                           , "subcmd" DA..= sc
                                                           , "cname" DA..= cn
                                                           , "qname" DA..= qn
                                                           , "file" DA..= f
                                                           , "userdir" DA..= u
                                                           , "username" DA..= un 
                                                           , "password" DA..= ps
                                                           , "debug_input" DA..= di]

instance DA.FromJSON ResAtSubmit where
 parseJSON (DA.Object v) = ResAtSubmit <$> (v DA..: "resstatus")
                                       <*> (v DA..: "resmsg")
                                       <*> (v DA..: "resresult")

instance DA.ToJSON ResAtSubmit where
 toJSON (ResAtSubmit rs rm rr) = DA.object [ "resstatus" DA..= rs
                                           , "resmsg" DA..= rm
                                           , "resresult" DA..= rr]

instance DA.FromJSON LJBase where parseJSON (DA.Object v) = LJBase <$> (v DA..: "language")
instance DA.ToJSON LJBase where toJSON (LJBase l) = DA.object [ "language" DA..= l ]

instance DA.FromJSON LangJson where
 parseJSON (DA.Object v) = LangJson <$> (v DA..: "name")
                                    <*> (v DA..: "extention")
                                    <*> (v DA..: "is_docker")
                                    <*> (v DA..:? "docker_image")
                                    <*> (v DA..:? "compile")
                                    <*> (v DA..:? "exec")
                                    <*> (v DA..: "langid")

instance DA.ToJSON LangJson where
 toJSON (LangJson n e id di c ex l) = DA.object [ "name" DA..= n
                                                , "extention" DA..= e
                                                , "is_docker" DA..= id
                                                , "docker_image" DA..= di
                                                , "compile" DA..= c 
                                                , "exec" DA..= c 
                                                , "langid" DA..= l]

nullContest = Contest { questions = [], cookie = [], csrf_token = "", homedir = "", main_file = ""
                      , input_file = "", compile_file = "", output_file = ""}
nullQuestion = Question { qurl = "", qio = []}
nullReqAtSubmit = ReqAtSubmit { rcom = "", subcmd = "", cname = Nothing, qname = Nothing, file = Nothing
                              , userdir = "", username = Nothing, password = Nothing, debug_input = []}
nullResAtSubmit = ResAtSubmit { resstatus = 100, resmsg = "nothing", resresult = []}
nullLangJson = LangJson { name = "", extention = [], is_docker = False, docker_image = Nothing
                        , compile = Nothing, exec = Nothing, langid = ""}

createContest :: V.Vector Question -> [BSC.ByteString] -> T.Text -> IO Contest
createContest q c t = getHomeDirectory >>= \d -> 
                      return Contest { questions = q, cookie = c, csrf_token = t, homedir = T.pack d
                                     , main_file = T.append (T.pack d) "/.cache/atsubmit/src/source.txt"
                                     , input_file = T.append (T.pack d) "/.cache/atsubmit/src/input.txt"
                                     , compile_file = T.append (T.pack d) "/.cache/atsubmit/src/comp.txt"
                                     , output_file = T.append (T.pack d) "/.cache/atsubmit/src/outres.txt"}

createQuestion :: T.Text -> V.Vector (T.Text, T.Text) -> Question
createQuestion url io = Question { qurl = url, qio = io}

createReqAtSubmit :: [T.Text] -> T.Text -> ReqAtSubmit
createReqAtSubmit r u = (parserAtSubmit r) {rcom = T.unwords r, userdir = u, username = Nothing, password = Nothing}
 where
  parserAtSubmit :: [T.Text] -> ReqAtSubmit
  parserAtSubmit [] = nullReqAtSubmit
  parserAtSubmit ("stop":l) = listAtSubmit ("stop":l) 1 -- atsubmit stop
  parserAtSubmit ("get":l) = listAtSubmit ("get":l) 2 -- atsubmit get [contest | question]
  parserAtSubmit ("show":l) = listAtSubmit ("show":l) 2 -- atsubmit show [ | question]
  parserAtSubmit ("submit":l) = listAtSubmit ("submit":l) 3 -- atsubmit submit question source
  parserAtSubmit ("test":l) = listAtSubmit ("test":l) 3 -- atsubmit test question source
  parserAtSubmit ("debug":l) = listAtSubmit ("debug":l) 4 -- atsubmit debug source inputfile
  parserAtSubmit ("login":l) = listAtSubmit ("login":l) 1 -- atsubmit login
  parserAtSubmit ("result":l) = listAtSubmit ("result":l) 2 -- atsubmit result [ | question]
  parserAtSubmit ("help":l) = listAtSubmit ("help":l) 1 -- atsubmit help
  parserAtSubmit _ = nullReqAtSubmit
  listAtSubmit :: [T.Text] -> Int -> ReqAtSubmit
  listAtSubmit l n = case (Prelude.length l, n) of
   (1, 1) -> nullReqAtSubmit { subcmd = l !! 0, cname = Nothing, qname = Nothing, file = Nothing} 
-- max size = 2
   (2, 2) -> nullReqAtSubmit { subcmd = l !! 0, cname = Just (T.takeWhile (/= '_') (l !! 1))
                             , qname = getQName (l !! 1), file = Nothing}
   (1, 2) -> nullReqAtSubmit { subcmd = l !! 0, cname = Nothing, qname = Nothing, file = Nothing} 
-- max size = 3
   (2, 3) -> nullReqAtSubmit { subcmd = l !! 0, cname = Just (T.takeWhile (/= '_') (l !! 1))
                             , qname = getQName (l !! 1), file = Nothing}
   (3, 3) -> nullReqAtSubmit { subcmd = l !! 0, cname = Just (T.takeWhile (/= '_') (l !! 1))
                             , qname = getQName (l !! 1), file = Just (V.foldl1 T.append [u, "/", (l !! 2)])}
-- max size = 4
   (3, 4) -> nullReqAtSubmit { subcmd = l !! 0, cname = Nothing, qname = Nothing
                             , file = Just (V.foldl1 T.append [u, "/", (l !! 1)]), debug_input = l !! 2}
   _ -> nullReqAtSubmit
  getQName :: T.Text -> Maybe T.Text
  getQName txt = case T.find (=='_') txt of
   Nothing -> Nothing
   Just a  -> Just txt

createResAtSubmit :: Int -> T.Text -> [[T.Text]] -> ResAtSubmit
createResAtSubmit rs rm rr = ResAtSubmit { resstatus = rs, resmsg = rm, resresult = rr}

createResAtStatus :: Int -> T.Text -> ResAtSubmit
createResAtStatus n rm = nullResAtSubmit { resstatus = n, resmsg = rm }

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
      if (S.length msg) + i == (datasize size) then NSBS.send sock (toStrict "end") >> return [msg]
      else if (S.length msg) + i > (datasize size) then NSBS.send sock (toStrict "end") >> return [msg]
      else recvLoop k ((S.length msg) + i) >>= \next -> return (msg:next)
  _         -> return S.empty
 
sendMsg :: Socket -> S.ByteString -> Int -> IO ()
sendMsg sock msg n = do
 NSBS.send sock $ toStrict.DA.encode $ Sizes {socksize = n, datasize = S.length msg}
 json <- fromStrict <$> NSBS.recv sock n -- deside send size
 case DA.decode json of
      Just size -> sendLoop (takeNList (socksize size) msg)
      _         -> return ()
 where
  sendLoop :: [S.ByteString] -> IO()
  sendLoop m = do 
   NSBS.send sock $ Prelude.head m
   if Prelude.length m == 1 then NSBS.recv sock n >> return ()
   else sendLoop (Prelude.tail m)

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

scrapingCsrfToken :: BSC.ByteString -> T.Text
scrapingCsrfToken = decodeUtf8.(\x -> case x of 
 Nothing -> ""
 Just a -> BSC.drop 11 a).L.find (\x -> BSC.pack "csrf_token"==(BSC.take 10 x)).BSC.split '\NUL'.NUE.decodeByteString

languageSelect :: T.Text -> T.Text -> IO (LangJson)-- name, extention, docker_image, langid
languageSelect home fp = do
 json <- BSL.fromStrict <$> (BS.readFile.T.unpack.T.append home) langJson
 case DA.decode json :: Maybe LJBase of
  Nothing -> return nullLangJson
  Just lists -> do
   print $ language lists 
   case V.find (\i -> V.elem (getExtention fp) (extention i)) (language lists) of 
    Nothing   -> return nullLangJson
    Just lang -> return lang
 where
  getExtention :: T.Text -> T.Text
  getExtention = T.takeWhileEnd (\x -> x/='.')

checkResult :: [T.Text] -> [T.Text] -> Bool
checkResult [] []           = True
checkResult ([]:es) (ans)   = checkResult es ans
checkResult (r:es) (a:ns)   = if r == a then checkResult es ns else False
checkResult _ _             = False

useDockerTest :: Maybe T.Text -> T.Text -> Contest -> IO (Maybe Int)
useDockerTest (Just image) main contest = do
 shell (V.foldl1 T.append ["docker create --name atsubmit_run --pids-limit 10 --network \"none\" ", image]) Turtle.empty
 shell (V.foldl1 T.append ["docker cp ", main_file contest, " atsubmit_run:/home/", main]) Turtle.empty
 shell (V.foldl1 T.append ["docker cp ", input_file contest, " atsubmit_run:/home/input.txt"]) Turtle.empty
 shell "docker start atsubmit_run" Turtle.empty
 ec <- shell "timeout 2 docker wait atsubmit_run" Turtle.empty
 timecheck <- Turtle.fold (inshell "docker inspect atsubmit_run --format=\'{{.State.ExitCode}}\'" Turtle.empty) CF.head
 shell (V.foldl1 T.append ["docker cp atsubmit_run:/home/output.txt ", output_file contest]) Turtle.empty
 shell (V.foldl1 T.append ["docker cp atsubmit_run:/home/comp.txt ", compile_file contest]) Turtle.empty
 shell "docker rm -f atsubmit_run" Turtle.empty
 case (ec,timecheck) of
  (ExitSuccess, Just "0") -> return $ Just 0
  (ExitSuccess, Just m)   -> return $ Just $ (read.T.unpack.lineToText) m
  (ExitFailure n, Just m) -> return $ Just $ if n == (read.T.unpack.lineToText) m then n else (read.T.unpack.lineToText) m 
useDockerTest _ _ _ = return Nothing 

unUseDocker :: Maybe T.Text -> Maybe T.Text -> T.Text -> Contest -> IO (Maybe Int)
unUseDocker (Just compcmd) (Just execmd) main contest = do
 copyFile ((T.unpack.main_file) contest) (T.unpack main)
 comp <- shell (V.foldl1 T.append [compcmd, " > ",compile_file contest, " 2>&1"]) Turtle.empty
 case comp of
  ExitFailure _ -> return $ Just 1
  ExitSuccess -> do
   exe <- shell (V.foldl1 T.append ["timeout 2 sh -c \"", execmd, " < ", input_file contest, " > ", output_file contest, "\""]) Turtle.empty
   case exe of
    ExitFailure 124 -> return $ Just 124
    ExitFailure n   -> return $ Just 2
    ExitSuccess     -> return $ Just 0
unUseDocker _ _ _ _ = return Nothing 
