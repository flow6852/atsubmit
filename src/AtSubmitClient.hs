{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module AtSubmitClient where

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

client :: [T.Text] -> Socket -> IO()
client msg sock = do
 cwd <- T.pack <$> getCurrentDirectory
 let req = createReqAtSubmit msg cwd
 NSBS.send sock $ toStrict.DA.encode $ req
 json <- fromStrict <$> NSBS.recv sock 1024
 TIO.putStrLn (case DA.decode json :: Maybe ResAtSubmit of
  Nothing -> "json parse error"
  Just x  -> case (subcmd req, resstatus x) of
                  ("stop", 200)   ->  resmsg x
                  ("get", 200)    ->  resmsg x
                  ("show", 200)   ->  T.intercalate "\n".(if qname req == Nothing then Prelude.head else atShowRes) $ resresult x
                  ("submit", 200) ->  resmsg x
                  ("test", 200)   ->  testShow.resresult $ x
                  ("login", 200)  ->  resmsg x
                  ("result", 200) ->  T.intercalate "\n".Prelude.map (T.intercalate " : ") $ resresult x
                  ("help", 200)   ->  Prelude.head.Prelude.head.resresult $ x
                  _               ->  V.foldl1 T.append [(T.pack.show.resstatus) x, " : ", resmsg x])

atShowRes :: [[T.Text]] -> [T.Text]
atShowRes x = Prelude.zipWith (\a b -> V.foldl1 T.append ["======= case ",(T.pack.show) a, " =======\n", b]) [1..(Prelude.length x)]
                              (Prelude.map (\[i,o] -> T.intercalate "\n" ["===== input =====", i, "===== output =====", o]) x)

testShow :: [[T.Text]] -> T.Text
testShow q = T.intercalate "\n".Prelude.map (T.intercalate "\n".resultMsg) $ q
 where
  resultMsg :: [T.Text] -> [T.Text]
  resultMsg [a,b]     = [V.foldl1 T.append ["case ", a, " : ", b]]
  resultMsg [a,b,c]   = [V.foldl1 T.append ["case ", a, " : ", b], "===== compile message =====", c]
  resultMsg [a,b,c,d] = [V.foldl1 T.append ["case ", a, " : ", b], "=== output ===", c, "=== test case ===", d] 
