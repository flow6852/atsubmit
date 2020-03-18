{-# LANGUAGE OverloadedStrings #-}

module Server.ShowPage where

import Lib

import qualified Data.Text as T
import qualified Data.Vector as V 

atShowPage :: AtFunc
atShowPage contests msg = case (cname msg, qname msg) of
 (Nothing, Nothing) -> return $ Right (contests, createResAtSubmit 200 "all show." [(atAllShow.questions) contests])
 (Just cm, Just qm) -> do
  let mquest = V.find ((== qm).T.takeWhileEnd (/='/').qurl) $ questions contests
  case mquest of Nothing -> return $ Left (405, "not get.")
                 Just a  -> return $ Right (contests, createResAtSubmit 200 "accept show" ((V.toList.V.map ltot.qio) a))
 _                  -> return $ Left (405, "nothing contest name")
 where
  ltot :: (T.Text, T.Text) -> [T.Text]
  ltot (a, b) = [a, b]

atAllShow :: V.Vector Question -> [T.Text]
atAllShow = V.toList.V.map (T.takeWhileEnd (/='/').qurl)

