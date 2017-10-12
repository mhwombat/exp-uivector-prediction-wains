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

data Action = Add Word16
  deriving (Show, Read, Eq, Ord, Generic)

mkAction :: Word16 -> Action
mkAction x
  | x > maxIncrement = error "argument too big"
  | otherwise        = Add x

instance Serialize Action
instance Genetic Action
instance Diploid Action
instance Pretty Action

instance Enum Action where
  toEnum n = mkAction $ toEnum n
  fromEnum (Add x) = fromEnum x

instance Bounded Action where
  minBound = Add 0
  maxBound = Add maxIncrement

instance Random Action where
  randomR (Add x, Add y) g = (Add z, g')
    where (z, g') = randomR (x, y) g
  random g = (Add z, g')
    where (z, g') = random g

predict :: Action -> UIDouble -> UIDouble
predict (Add z) x
  = forceDoubleToUI $ (fromIntegral z - mid)*aBit + (uiToDouble x)
  where aBit = 2 / fromIntegral maxIncrement
        mid  = fromIntegral maxIncrement / 2

postdict :: UIDouble -> UIDouble -> Action
postdict x1 x2 = Add z
  where z = round $ (delta + 1)*m/2
        delta = uiToDouble x2 - uiToDouble x1
        m = fromIntegral maxIncrement

actionDiff :: Action -> Action -> Difference
actionDiff (Add x) (Add y)
  = doubleToUI $
      abs (fromIntegral x - fromIntegral y) / fromIntegral maxIncrement

makeActionSimilar :: Action -> UIDouble -> Action -> Action
makeActionSimilar (Add x) r (Add y)
  = Add $ adjustIntegral x r y
