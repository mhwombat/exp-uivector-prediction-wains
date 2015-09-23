------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.Action
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ?????
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module ALife.Creatur.Wain.Prediction.Action
  (
    Action(..),
    predict,
    numActions
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiApply)
import ALife.Creatur.Wain.Util (unitInterval, enforceRange)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)

-- The actions are listed in order of decreasing genetic dominance.
data Action = Down100 | Down095 | Down090 | Down085 | Down080 | Down075
                | Down070 | Down065 | Down060 | Down055 | Down050
                | Down045 | Down040 | Down035 | Down030 | Down025
                | Down020 | Down015 | Down010 | Down005 | Steady
                | Up005 | Up010 | Up015 | Up020 | Up025 | Up030 | Up035
                | Up040 | Up045 | Up050 | Up055 | Up060 | Up065 | Up070
                | Up075 | Up080 | Up085 | Up090 | Up095 | Up100
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Serialize Action
instance Genetic Action
instance Diploid Action
instance Pretty Action

instance Random Action where
  randomR (a,b) g = (toEnum n, g')
    where (n, g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound,maxBound)

predict :: Action -> UIDouble -> UIDouble
predict Down100 = uiApply (\x -> enforceRange unitInterval (x - 1))
predict Down095 = uiApply (\x -> enforceRange unitInterval (x - 0.95))
predict Down090 = uiApply (\x -> enforceRange unitInterval (x - 0.90))
predict Down085 = uiApply (\x -> enforceRange unitInterval (x - 0.85))
predict Down080 = uiApply (\x -> enforceRange unitInterval (x - 0.80))
predict Down075 = uiApply (\x -> enforceRange unitInterval (x - 0.75))
predict Down070 = uiApply (\x -> enforceRange unitInterval (x - 0.70))
predict Down065 = uiApply (\x -> enforceRange unitInterval (x - 0.65))
predict Down060 = uiApply (\x -> enforceRange unitInterval (x - 0.60))
predict Down055 = uiApply (\x -> enforceRange unitInterval (x - 0.55))
predict Down050 = uiApply (\x -> enforceRange unitInterval (x - 0.50))
predict Down045 = uiApply (\x -> enforceRange unitInterval (x - 0.45))
predict Down040 = uiApply (\x -> enforceRange unitInterval (x - 0.40))
predict Down035 = uiApply (\x -> enforceRange unitInterval (x - 0.35))
predict Down030 = uiApply (\x -> enforceRange unitInterval (x - 0.30))
predict Down025 = uiApply (\x -> enforceRange unitInterval (x - 0.25))
predict Down020 = uiApply (\x -> enforceRange unitInterval (x - 0.20))
predict Down015 = uiApply (\x -> enforceRange unitInterval (x - 0.15))
predict Down010 = uiApply (\x -> enforceRange unitInterval (x - 0.10))
predict Down005 = uiApply (\x -> enforceRange unitInterval (x - 0.05))
predict Steady = id
predict Up005 = uiApply (\x -> enforceRange unitInterval (x + 0.05))
predict Up010 = uiApply (\x -> enforceRange unitInterval (x + 0.10))
predict Up015 = uiApply (\x -> enforceRange unitInterval (x + 0.15))
predict Up020 = uiApply (\x -> enforceRange unitInterval (x + 0.20))
predict Up025 = uiApply (\x -> enforceRange unitInterval (x + 0.25))
predict Up030 = uiApply (\x -> enforceRange unitInterval (x + 0.30))
predict Up035 = uiApply (\x -> enforceRange unitInterval (x + 0.35))
predict Up040 = uiApply (\x -> enforceRange unitInterval (x + 0.40))
predict Up045 = uiApply (\x -> enforceRange unitInterval (x + 0.45))
predict Up050 = uiApply (\x -> enforceRange unitInterval (x + 0.50))
predict Up055 = uiApply (\x -> enforceRange unitInterval (x + 0.55))
predict Up060 = uiApply (\x -> enforceRange unitInterval (x + 0.60))
predict Up065 = uiApply (\x -> enforceRange unitInterval (x + 0.65))
predict Up070 = uiApply (\x -> enforceRange unitInterval (x + 0.70))
predict Up075 = uiApply (\x -> enforceRange unitInterval (x + 0.75))
predict Up080 = uiApply (\x -> enforceRange unitInterval (x + 0.80))
predict Up085 = uiApply (\x -> enforceRange unitInterval (x + 0.85))
predict Up090 = uiApply (\x -> enforceRange unitInterval (x + 0.90))
predict Up095 = uiApply (\x -> enforceRange unitInterval (x + 0.95))
predict Up100 = uiApply (\x -> enforceRange unitInterval (x + 1))

numActions :: Int
numActions = 1 + fromEnum (maxBound :: Action)
