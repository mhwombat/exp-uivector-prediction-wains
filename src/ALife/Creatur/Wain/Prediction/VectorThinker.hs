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
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble,
  doubleToUI)
import ALife.Creatur.Wain.Weights (Weights, toDoubles)
import Data.Datamining.Pattern (adjustVector)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data VectorThinker = VectorThinker Weights
  deriving (Eq, Show, Generic)

instance Thinker VectorThinker where
  type Pattern VectorThinker = [UIDouble]
  diff (VectorThinker ws) = weightedUiVectorDiff ws
  adjust _ = makeUIVectorsSimilar

instance Serialize VectorThinker
instance W8.Genetic VectorThinker
instance Diploid VectorThinker

weightedUiVectorDiff :: Weights -> [UIDouble] -> [UIDouble] -> Double
weightedUiVectorDiff ws xs ys
  | null xs && null ys = 0
  | null xs || null ys = 1
  | otherwise         = weightedDiff ws' xs' ys'
  where xs' = map uiToDouble xs
        ys' = map uiToDouble ys
        ws' = toDoubles ws

weightedDiff :: [Double] -> [Double] -> [Double] -> Double
weightedDiff ws xs ys
  = sum . zipWith (*) ws . map abs $ zipWith (-) xs ys

makeUIVectorsSimilar :: [UIDouble] -> Double -> [UIDouble] -> [UIDouble]
makeUIVectorsSimilar xs r ys = map doubleToUI $ adjustVector xs' r ys'
  where xs' = map uiToDouble xs
        ys' = map uiToDouble ys
