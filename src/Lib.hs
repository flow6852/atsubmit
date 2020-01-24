module Lib where

import qualified Data.Text as T
import System.IO
import qualified Data.ByteString.Char8 as BSC

data UserData = UserData { username :: T.Text
                         , password :: T.Text
                         , csrf_token :: T.Text
                         , cookie :: [BSC.ByteString]} deriving (Show)


nullUserData = UserData { username = T.empty, password = T.empty, csrf_token = T.empty, cookie = []}

chromiumSession = "~/.config/chromium/Default/Current Session"

createUserData :: String -> String -> UserData
createUserData un ps = UserData { username = T.pack un, password = T.pack ps, csrf_token = T.empty, cookie = []}

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

