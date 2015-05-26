------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.DataGen
-- Copyright   :  (c) Amy de Buitléir 2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tool to generate data for wain demos.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Prediction.DataGen where

import qualified ALife.Creatur as A
import qualified ALife.Creatur.Database as D
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI)
import qualified ALife.Creatur.Wain.Prediction.Universe as U
--import Control.Monad.Random (Rand, RandomGen, evalRandIO, getRandomR)
import Control.Monad.State.Lazy (StateT)

-- We want four records per hour, so that's 360 records in a day.
recordNumToRadians :: Int -> Double
recordNumToRadians x = (2 * pi * fromIntegral x)/360

-- genRecord :: RandomGen r => Int -> Rand r [Double]
-- genRecord t = do
--   let t' = recordNumToRadians t
--   let v1 = 15*sin t'
--   let v2 = 3*(cos 3*t')
--   let v3 = 54*v1*v1 + 3*v2*v2
--   let v4 = 10*v1 + 6*v2 + v3
--   let v5 = -33*v3 + v1
--   let vs = [v1, v2, v3, v4, v5]
--   ys <- mapM getRandomR [(-3,3), (-0.2,0.2), (-5,5), (-18,18), (-6,6)]
--   return $ fromIntegral t : zipWith (+) vs ys

-- genRecords :: RandomGen r => Int -> Rand r [[Double]]
-- genRecords n = mapM genRecord [0,1..n-1]

-- formatRecord :: [Double] -> String
-- formatRecord = concat . intersperse "," . map show

-- main :: IO ()
-- main = do
--   putStrLn "time,w,v,x,y,z"
--   rs <- evalRandIO $ genRecords 10
--   let ss = map formatRecord rs
--   putStrLn $ unlines ss

nextVector :: (A.Agent w, D.SizedRecord w) => StateT (U.Universe w) IO [UIDouble]
nextVector = do
  t <- U.currentTime
  let xs = [doubleToUI . sin . recordNumToRadians $ t, doubleToUI . fromIntegral $ t `mod` 360]
  U.writeToLog $ "Next record to mine: " ++ show xs
  return xs
