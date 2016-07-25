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
    Action(..),
    predict,
    actionDiff,
    makeActionSimilar
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty)
import ALife.Creatur.Wain.GeneticSOM (Difference)
import ALife.Creatur.Wain.UnitInterval (UIDouble, forceDoubleToUI,
  uiToDouble, diffIntegral, adjustIntegral)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)

-- The actions are listed in order of decreasing genetic dominance.
data Action = MultiplyBy Word8
  deriving (Show, Read, Eq, Ord, Bounded, Generic)

instance Serialize Action
instance Genetic Action
instance Diploid Action
instance Pretty Action

instance Enum Action where
  toEnum n = MultiplyBy $ toEnum n
  fromEnum (MultiplyBy x) = fromEnum x

instance Random Action where
  randomR (MultiplyBy x, MultiplyBy y) g = (MultiplyBy z, g')
    where (z, g') = randomR (x, y) g
  random g = (MultiplyBy z, g')
    where (z, g') = random g

predict :: Action -> UIDouble -> UIDouble
predict (MultiplyBy z) x
  = forceDoubleToUI $ ((fromIntegral z :: Double)/100) * (uiToDouble x)

actionDiff :: Action -> Action -> Difference
actionDiff (MultiplyBy x) (MultiplyBy y) = diffIntegral x y

makeActionSimilar :: Action -> UIDouble -> Action -> Action
makeActionSimilar (MultiplyBy x) r (MultiplyBy y)
  = MultiplyBy $ adjustIntegral x r y
