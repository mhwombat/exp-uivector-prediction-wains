------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Prediction.Action
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ?????
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module ALife.Creatur.Wain.UIVector.Prediction.Action
  (
    Action,
    mkAction,
--    mkAddAction,
--    mkMultiplyAction,
    predict,
    postdict,
    actionDiff,
    makeActionSimilar
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty)
import ALife.Creatur.Wain.GeneticSOM (Difference)
import ALife.Creatur.Wain.UnitInterval (UIDouble, forceDoubleToUI,
  uiToDouble, doubleToUI, adjustIntegral)
import Data.Serialize (Serialize)
import Data.Word (Word16)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)

maxIncrement :: Word16
maxIncrement = 1023


--maxValue :: Int
--maxValue = 1023

--maxEnum :: Int
--maxEnum = 2*maxValue + 1

data Action = Add Word16
  -- | Multiply Word16
  deriving (Show, Read, Eq, Ord, Generic)

mkAction :: Word16 -> Action
mkAction x
  | x > maxIncrement = error "argument too big"
  | otherwise        = Add x

--mkAddAction :: Word16 -> Action
--mkAddAction x
--  | x > fromIntegral maxValue = error "argument too big"
--  | otherwise                 = Add x

--mkMultiplyAction :: Word16 -> Action
--mkMultiplyAction x
--  | x > fromIntegral maxValue = error "argument too big"
--  | otherwise                 = Multiply x

instance Serialize Action
instance Genetic Action
instance Diploid Action
instance Pretty Action

instance Enum Action where
  toEnum n = mkAction $ toEnum n
  fromEnum (Add x) = fromEnum x

--instance Enum Action where
--  toEnum n | n <= maxValue  = mkAddAction $ toEnum n
--           | n <= maxEnum  = mkMultiplyAction $ toEnum (n - maxValue - 1)
--           | otherwise = error "argument too big"
--  fromEnum (Add x) = fromEnum x
--  fromEnum (Multiply x) = maxValue + 1 + fromEnum x

instance Bounded Action where
  minBound = Add 0
  maxBound = Add maxIncrement
--  maxBound = Multiply (fromIntegral maxValue)

instance Random Action where
  randomR (a, b) g = (toEnum n, g')
    where (n, g') = randomR (fromEnum a, fromEnum b) g
  random g = (toEnum n, g')
    where (n, g') = randomR (0, maxEnum) g
          maxEnum = fromEnum (maxBound :: Action)

predict :: Action -> UIDouble -> UIDouble
predict (Add z) x
  = forceDoubleToUI $ uiToDouble x + delta
  where delta = (fromIntegral z)*2/m - 1
        m = fromIntegral maxIncrement
--        m = fromIntegral maxValue
--predict (Multiply z) x
--  = forceDoubleToUI $ uiToDouble x * factor
--  where factor = fromIntegral z / 100

postdict :: UIDouble -> UIDouble -> Action
postdict x1 x2 = Add z
  where z = round $ (delta + 1)*m/2
        delta = uiToDouble x2 - uiToDouble x1
        m = fromIntegral maxIncrement
--        m = fromIntegral maxValue

actionDiff :: Action -> Action -> Difference
actionDiff (Add x) (Add y)
  = doubleToUI $
      abs (fromIntegral x - fromIntegral y) / fromIntegral maxIncrement
--      abs (fromIntegral x - fromIntegral y) / fromIntegral maxValue
--actionDiff (Multiply x) (Multiply y)
--  = doubleToUI $
--      abs (fromIntegral x - fromIntegral y) / fromIntegral maxValue
--actionDiff _ _ = 1

makeActionSimilar :: Action -> UIDouble -> Action -> Action
makeActionSimilar a r b
  = toEnum $ adjustIntegral n r m
  where n = fromEnum a
        m = fromEnum b
