{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module AtCoderLib where

import Lib

import Data.Text.Encoding
import Data.ByteString.Lazy.Internal
import qualified Network.Socket.ByteString as NSBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy
import Control.Monad
import qualified Data.Vector as V
import qualified Text.XML.Cursor as TXC
import qualified Text.HTML.DOM as THD
import System.Directory
import Control.Applicative
import Control.Exception
import qualified Data.Aeson as DA

server :: Socket -> Contest -> IO (Bool, Contest)
server sock contests = do
 json <- fromStrict <$> NSBS.recv sock 1024 
 case DA.decode json of
  Nothing -> NSBS.send sock ((encodeUtf8.T.pack) "parsing error")  >> return (False, contests)
  Just x  -> do
   let (func, retb) =  case (T.unpack.subcmd) x of
                            "stop"   -> (atLogout, True)
                            "get"    -> (atGetPage, False)
                            "show"   -> (atShowPage, False)
                            "submit" -> (atSubmit, False)
                            "test"   -> (atTest, False)
                            "login"  -> (atLogin, False)
                            "result" -> (atResult, False)
                            "help"   -> (atHelp, False)
                            _        -> (notDo, False)
   (res, retc) <- func contests x
                   `catch` (\e -> return (createResAtSubmit (-1) [] [] (Just ((T.pack.displayException) (e :: SomeException))), contests))
   NSBS.send sock $ toStrict.DA.encode $ res
   return (retb, retc)
    where
     notDo :: AtFunc
     notDo c m = return (createResAtStatus (-1), c)

client :: [T.Text] -> Socket -> IO()
client msg sock = do
 cwd <- T.pack <$> getCurrentDirectory
 NSBS.send sock $ toStrict.DA.encode $ createReqAtSubmit msg cwd
 json <- fromStrict <$> NSBS.recv sock 1024
 print json
-- print $ case DA.decode json of Nothing -> "json parse error"
--                                Just x  -> x 

atLogin :: AtFunc
atLogin contests msg = do 
 [user, pass] <- getAtKeys 
 next <- getCookieAndCsrfToken (T.pack user) (T.pack pass)
 let !res = if Prelude.null (cookie next) then createResAtStatus 1 else createResAtStatus 0
 return (res, next)

atGetPage :: AtFunc 
atGetPage contests msg = case qname msg of
 Nothing -> return (createResAtStatus 1 , contests) -- json error
 Just qm -> if V.elem qm (V.map (T.takeWhileEnd (/='/').qurl) (questions contests)) then return (createResAtStatus 2, contests)
            else do -- already get
               quest <- getPageInfo msg contests
               return $ if quest == nullQuestion then (createResAtStatus 3, contests)  -- nothing
                        else (createResAtSubmit 0 [] [] Nothing, contests {questions = V.snoc (questions contests) quest})

atShowPage :: AtFunc
atShowPage contests msg = case qname msg of
 Nothing -> return (createResAtSubmit 0 ((atAllShow.questions) contests) [] Nothing, contests)
 Just qm -> do 
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  let showPage = case mquest of Nothing -> createResAtStatus 1
                                Just a  -> createResAtSubmit 0 ((V.toList.qio) a) [] Nothing
  return (showPage, contests)
   where
    showMsg :: V.Vector (T.Text, T.Text) -> V.Vector T.Text
    showMsg q = V.zipWith (\a b -> V.foldl1 T.append ["======= case ", (T.pack.show) a, " =======\n", b]) [1..(V.length q)]
                          (V.map (\x -> T.intercalate "\n" ["===== input =====", fst x, "===== output =====", snd x]) q)

atAllShow :: V.Vector Question  -> [(T.Text, T.Text)]
atAllShow [] = []
atAllShow q  = ((T.takeWhileEnd (/='/').qurl.V.head) q, ""):((atAllShow.V.tail) q)

atSubmit :: AtFunc
atSubmit contests msg = postSubmit msg contests >> return (createResAtStatus 0, contests)

atResult :: AtFunc
atResult contests msg = case cname msg of
 Just cm -> do
  res <- getContestResult cm contests
  return (createResAtSubmit 0 [] res Nothing, contests) -- only contest result
 Nothing -> if V.null (questions contests) then return (createResAtStatus 2, contests) else do -- nothing questions
  res <- loop ((rmDup.V.map (T.takeWhile (/='_').T.takeWhileEnd (/='/').qurl)) (questions contests)) contests -- all result
  return (createResAtSubmit 1 [] (Prelude.concat res) Nothing, contests) -- all result
   where
    loop :: V.Vector T.Text -> Contest -> IO [[(T.Text, T.Text, T.Text)]]
    loop quest cont = if V.null quest then return [] else do
     res <- getContestResult (V.head quest) cont
     bef <- loop (V.tail quest) cont
     return $ res:bef

atTest :: AtFunc
atTest contests msg = case (qname msg, file msg) of
 (Just qm, Just fm) -> do
  home <- getHomeDirectory
  TIO.readFile (T.unpack (T.append (userdir msg) (T.append (T.singleton '/') fm))) >>= TIO.writeFile (home ++ mainfile)
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  (resint, result) <- case mquest of Nothing -> return (2, []) -- not getting
                                     Just a  -> testLoop (qio a) home 1 >>= \x -> return (0, x)
  return (createResAtSubmit resint [] result Nothing, contests)
 _  -> return (createResAtStatus 1 , contests) -- nothing question
 where
  mainfile = "/.cache/atsubmit/src/source.txt"

atLogout :: AtFunc
atLogout contests msg = postLogout contests >>= \x -> return (createResAtStatus x, nullContest)

atHelp :: AtFunc
atHelp contests msg = TIO.readFile helpFile >>= \x -> return (createResAtSubmit 0 [] [] (Just x), contests)
