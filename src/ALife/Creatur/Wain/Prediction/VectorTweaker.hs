------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.VectorTweaker
-- Copyright   :  (c) Amy de Buitléir 2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for comparing and adjusting images.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Prediction.VectorTweaker
  (
    VectorTweaker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Tweaker(..))
import ALife.Creatur.Wain.UnitInterval (UIDouble, adjustUIVector)
import ALife.Creatur.Wain.Weights (Weights, weightedUIVectorDiff)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data VectorTweaker = VectorTweaker Weights
  deriving (Eq, Show, Generic)

instance Tweaker VectorTweaker where
  type Pattern VectorTweaker = [UIDouble]
  diff (VectorTweaker ws) = weightedUIVectorDiff ws
  adjust _ = adjustUIVector

instance Serialize VectorTweaker
instance W8.Genetic VectorTweaker
instance Diploid VectorTweaker
