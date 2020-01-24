module Lib where

import qualified Data.Text as T
import System.IO
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V

data Question = Question { qurl :: T.Text -- question page's url
                         , qio :: V.Vector (T.Text, T.Text) -- input, output
                         } deriving (Show)

data Contest = Contest { questions :: V.Vector Question 
                       , cookie :: [BSC.ByteString]
                       , csrf_token :: T.Text} deriving (Show)

nullContest = Contest { questions = V.empty, cookie = [], csrf_token = T.empty}
nullQuestion = Question { qurl = T.empty, qio = V.empty}

chromiumSession = "~/.config/chromium/Default/Current Session"

createContest :: V.Vector Question -> [BSC.ByteString] -> T.Text -> Contest
createContest q c t = Contest { questions = q, cookie = c, csrf_token = t}

createQuestion :: T.Text -> V.Vector (T.Text, T.Text) -> Question
createQuestion url io = Question { qurl = url, qio = io }

getAPIkeys :: [String] -> IO [String]
getAPIkeys [] = return []
getAPIkeys (m:messages) = do
 Prelude.putStr m 
 hFlush stdout
 api <- Prelude.getLine 
 Prelude.putChar '\n'
 getAPIkeys messages >>= (\res -> return (api:res))

getAtKeys :: IO [String]
getAtKeys = do
 hSetEcho stdin False
 System.IO.putStrLn "============== atcoder username and password ==============="
 apis <- getAPIkeys ["username : ", "password : "]
 hSetEcho stdin True
 return apis

