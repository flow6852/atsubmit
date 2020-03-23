{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module Client where

import Lib

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket
import Data.ByteString.Lazy
import qualified Data.Vector as V
import System.Directory
import Control.Exception as E
import qualified Data.Aeson as DA

sendServer :: FilePath -> (Socket -> IO()) -> IO()
sendServer path client = withSocketsDo $ E.bracket (open path) close client
 where
  open :: FilePath -> IO Socket
  open path = do
   s <- socket AF_UNIX Stream 0
   connect s (SockAddrUnix path)
   return s

client :: [T.Text] -> Socket -> IO()
client msg sock = do
 cwd <- T.pack <$> getCurrentDirectory
 let request = (createReqAtSubmit msg cwd) 
 req <- case subcmd request of 
             "login" ->  (\[user, pass] -> request {username = Just (T.pack user), password = Just (T.pack pass)}) <$> getAtKeys
             _       ->  return request
 sendMsg sock ((toStrict.DA.encode) req) 1024
 json <- fromStrict <$> recvMsg sock 1024
 case DA.decode json :: Maybe ResAtSubmit of
  Nothing -> TIO.putStrLn "responce : json parse error"
  Just x  ->
   case (subcmd req, resstatus x) of
    ("stop", 200)   -> (TIO.putStrLn.resmsg) x
    ("get", 200)    -> (TIO.putStrLn.T.intercalate "\n".Prelude.concat.resresult) x
    ("show", 200)   -> TIO.putStrLn.T.intercalate "\n".(if qname req == Nothing then Prelude.head else atShowRes).resresult $ x
    ("submit", 200) -> (TIO.putStrLn.resmsg) x
    ("test", _)     -> testShow sock (Just x)
    ("login", 200)  -> (TIO.putStrLn.resmsg) x
    ("result", 200) -> (TIO.putStrLn.T.intercalate "\n".Prelude.map (T.intercalate " : ").resresult) x
    ("help", 200)   -> (TIO.putStrLn.Prelude.head.Prelude.head.resresult) x
    _               -> (TIO.putStrLn.V.foldl1 T.append) [(T.pack.show.resstatus) x, " : ", resmsg x]

atShowRes :: [[T.Text]] -> [T.Text]
atShowRes x = Prelude.zipWith (\a b -> V.foldl1 T.append ["======= case ",(T.pack.show) a, " =======\n", b]) [1..(Prelude.length x)]
                              (Prelude.map (\[i,o] -> T.intercalate "\n" ["===== input =====", i, "===== output =====", o]) x)

testShow :: Socket -> Maybe ResAtSubmit -> IO ()
testShow _ Nothing = TIO.putStrLn "json error."
testShow sock (Just q) = do
 TIO.putStrLn.T.intercalate "\n".Prelude.map (T.intercalate "\n".resultMsg).resresult $ q
 next <- (DA.decode.fromStrict) <$> recvMsg sock 1024
 case resmsg q of "last" -> lastRecv next 
                  _      -> testShow sock next
  where
   resultMsg :: [T.Text] -> [T.Text]
   resultMsg [a,b]     = [V.foldl1 T.append ["case ", a, " : ", b]]
   resultMsg [a,b,c]   = [V.foldl1 T.append ["case ", a, " : ", b], "===== compile message =====", c]
   resultMsg [a,b,c,d] = [V.foldl1 T.append ["case ", a, " : ", b], "=== output ===", c, "=== test case ===", d] 
   lastRecv :: Maybe ResAtSubmit -> IO ()
   lastRecv Nothing   = TIO.putStrLn "last json error"
   lastRecv (Just lj) = (TIO.putStrLn.resmsg) lj
