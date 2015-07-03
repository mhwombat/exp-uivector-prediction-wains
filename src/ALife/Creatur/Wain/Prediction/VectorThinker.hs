------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.VectorThinker
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
module ALife.Creatur.Wain.Prediction.VectorThinker
  (
    VectorThinker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Thinker(..))
import ALife.Creatur.Wain.UnitInterval (UIDouble, adjustUIVector)
import ALife.Creatur.Wain.Weights (Weights, weightedUIVectorDiff)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data VectorThinker = VectorThinker Weights
  deriving (Eq, Show, Generic)

instance Thinker VectorThinker where
  type Pattern VectorThinker = [UIDouble]
  diff (VectorThinker ws) = weightedUIVectorDiff ws
  adjust _ = adjustUIVector

instance Serialize VectorThinker
instance W8.Genetic VectorThinker
instance Diploid VectorThinker
