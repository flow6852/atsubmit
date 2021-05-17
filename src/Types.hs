{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Data.Aeson.TH
import Control.Exception
import System.Environment
import qualified Data.ByteString.Char8 as BSC
import Data.Typeable
import Network.Socket

newtype Username = Username T.Text deriving (Show, Eq)
newtype Password = Password T.Text deriving (Show, Eq)
newtype QName = QName T.Text deriving (Show, Eq)
newtype CName = CName T.Text deriving (Show, Eq)
newtype Source = Source FilePath deriving (Show, Eq)
newtype Userdir = Userdir FilePath deriving (Show, Eq)
newtype QUrl = QUrl T.Text deriving (Show, Eq)
newtype QIO = QIO (V.Vector (T.Text, T.Text)) deriving (Show, Eq)
newtype DIn = DIn FilePath deriving (Show, Eq)
newtype DOut = DOut T.Text deriving (Show, Eq)
newtype TOut = TOut T.Text deriving (Show, Eq)
newtype TAns = TAns T.Text deriving (Show, Eq)
newtype Message = Message T.Text deriving (Show, Eq)
newtype CResult = CResult [[T.Text]] deriving (Show, Eq)
newtype RLog = RLog (SHelperServerRequest, SHelperServerResponse) deriving (Show, Eq)
newtype Lang = Lang T.Text deriving (Show, Eq)
newtype Id = Id T.Text deriving (Show, Eq)
newtype LanguageId = LanguageId (Id, Lang) deriving (Show, Eq)
data GetResult = GetResultOk QName 
               | FromLocal QName
               | AlreadyGet QName
               | QuestionNotExist QName
               | ContestNotExist CName
               | Other
               deriving (Show, Eq)

data Question = Question { qurl :: T.Text
                         , qsentence :: T.Text
                         , qrestriction :: V.Vector T.Text
                         , qio :: (T.Text, T.Text)
                         , qiosample  :: V.Vector (T.Text, T.Text)
                         } deriving (Show, Eq)

data Contest = Contest { questions :: V.Vector Question 
                       , csrf_token :: T.Text
                       , rlogs :: V.Vector RLog
                       , homedir :: T.Text
                       , main_file :: T.Text
                       , input_file :: T.Text
                       , compile_file :: T.Text
                       , output_file :: T.Text
                       } deriving (Show, Eq)

-- 一回分のソケットサイズと送信するデータの大きさを束縛する
data  Sizes = Sizes { socksize :: Int
                    , datasize :: Int
                    } deriving (Show, Eq)

-- 自動提出の型.ユーザが可能なアクション.
-- リクエストを受け取ってレスポンスを返す
data SHelper a where
        Login  :: Username -> Password -> SHelper () -- ユーザ名とパスワードを受け取ってログインする.ContestStateのcookieとscrf_tokenの更新.
        QGet   :: V.Vector QName -> Userdir -> SHelper (V.Vector GetResult) -- 問題名を受け取って問題を入手する.ContestStateのQuestionsを更新.
        CGet   :: V.Vector CName -> Userdir -> SHelper (V.Vector GetResult) -- コンテスト名を受け取ってそれに所属する問題のすべてを入手する.
        Test   :: Socket -> Source -> QName -> SHelper ()-- ファイルと問題名を受け取って結果を出力する. 
        Submit :: Source -> QName -> SHelper () -- ファイルと問題名を受け取って提出する.
        Debug  :: Source -> DIn -> SHelper DebugBodyRes -- ファイルと入力を受け取って出力を返す.
        Print  :: SHelper (V.Vector QName) -- ContestStateのQuestionsのすべてを返す.
        Show   :: QName -> SHelper Question -- 問題を受け取ってその問題入出力を返す.
        Result :: CName -> SHelper CResult -- コンテストを受け取ってコンテストの結果を出力する.
        Log    :: SHelper (V.Vector RLog) -- ログの出力
        LangId :: Lang -> SHelper (V.Vector LanguageId) -- data.LanguageIdの表示
        Stop   :: SHelper ()
        Logout :: SHelper ()

-- クライアントからのリクエスト."tag" でjsonの形式を切り替える.
data SHelperRequest 
        = LoginReq Username Password
        | QGetReq (V.Vector QName) Userdir
        | CGetReq (V.Vector CName) Userdir
        | TestReq Source QName
        | SubmitReq Source QName
        | DebugReq Source DIn
        | PrintReq
        | ShowReq QName
        | ResultReq CName
        | LogReq 
        | LangIdReq Lang
        | StopReq
        | LogoutReq
        deriving(Show, Eq)

-- サーバからの最終レスポンス. "tag"でjsonの形式を切り替える.
data SHelperResponse
        = LoginRes ()
        | QGetRes (V.Vector GetResult)
        | CGetRes (V.Vector GetResult)
        | TestRes ()
        | SubmitRes ()
        | DebugRes DebugBodyRes
        | PrintRes (V.Vector QName)
        | ShowRes Question
        | ResultRes CResult
        | LogRes (V.Vector RLog)
        | LangIdRes (V.Vector LanguageId)
        | StopRes ()
        | LogoutRes ()
        deriving (Show, Eq)

newtype SHelperServerRequest = SHelperServerRequest SHelperRequest deriving (Show, Eq)
data SHelperServerResponse = SHelperOk SHelperResponse | SHelperErr SHelperException deriving (Show, Eq)

data SHelperException
        = FailLogin
        | NotGetQuestion QName
        | BadData T.Text
        | JsonParseError
        | InputErr
        | InternalError
        | Unknown 
        deriving(Show, Typeable, Eq)

instance Exception SHelperException

-- サーバ,クライアント両用のエラーチェック. 
data CheckErr = Ok | Err T.Text deriving (Show, Eq)

data TestBodyRes
        = AC 
        | WA TOut TAns
        | CE Message
        | RE
        | TLE
        | IE
        deriving (Show, Eq)

data DebugBodyRes
        = DAC DOut
        | DCE Message
        | DRE
        | DTLE
        | DIE
        deriving (Show, Eq)

data LangJson = LangJson { name :: T.Text
                         , extention :: V.Vector T.Text
                         , is_docker :: Bool
                         , docker_image :: Maybe T.Text
                         , compile :: Maybe T.Text
                         , exec :: Maybe T.Text
                         , langid :: T.Text
                         } deriving (Show, Eq)
 
newtype LJBase = LJBase { language :: V.Vector LangJson } deriving (Show, Eq)


deriveJSON defaultOptions ''Username
deriveJSON defaultOptions ''Password
deriveJSON defaultOptions ''QName
deriveJSON defaultOptions ''CName
deriveJSON defaultOptions ''Source
deriveJSON defaultOptions ''Userdir
deriveJSON defaultOptions ''QUrl
deriveJSON defaultOptions ''QIO
deriveJSON defaultOptions ''DIn
deriveJSON defaultOptions ''DOut
deriveJSON defaultOptions ''TOut
deriveJSON defaultOptions ''TAns
deriveJSON defaultOptions ''Message
deriveJSON defaultOptions ''CResult
deriveJSON defaultOptions ''Question
deriveJSON defaultOptions ''Sizes
deriveJSON defaultOptions ''RLog
deriveJSON defaultOptions ''GetResult
deriveJSON defaultOptions ''Id
deriveJSON defaultOptions ''Lang
deriveJSON defaultOptions ''LanguageId
deriveJSON defaultOptions ''SHelperResponse
deriveJSON defaultOptions ''SHelperRequest
deriveJSON defaultOptions ''SHelperServerRequest
deriveJSON defaultOptions ''SHelperServerResponse
deriveJSON defaultOptions ''SHelperException
deriveJSON defaultOptions ''TestBodyRes
deriveJSON defaultOptions ''DebugBodyRes
deriveJSON defaultOptions ''LangJson
deriveJSON defaultOptions ''LJBase
