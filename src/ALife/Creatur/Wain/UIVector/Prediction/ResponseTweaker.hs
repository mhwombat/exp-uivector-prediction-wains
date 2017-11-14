------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Prediction.ResponseTweaker
-- Copyright   :  (c) Amy de Buitléir 2012-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tweaker for response models in the predictor.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.UIVector.Prediction.ResponseTweaker
  (
    ResponseTweaker(..),
    responseDiff,
    makeResponseSimilar
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Difference, Tweaker(..))
import ALife.Creatur.Wain.PlusMinusOne (adjustPM1Vector)
import  ALife.Creatur.Wain.Pretty (Pretty)
import ALife.Creatur.Wain.Response (Response(..), labelSimilarity)
import ALife.Creatur.Wain.UIVector.Prediction.Action (Action,
  actionDiff, makeActionSimilar)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.Weights (Weights, weightedSum)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | @'ResponseTweaker'@ constructs an object which is
--   responsible for comparing and adjusting response patterns.
--
--   NOTE: In order to declare an instance of @Tweaker@, we have to
--   reference the action type @a@. As a result, @ResponseTweaker@ has
--   to have a type parameter @a@, even though it is not used.
data ResponseTweaker = ResponseTweaker Weights
  deriving (Eq, Show, Pretty, Generic, NFData, Serialize, W8.Genetic,
            Diploid)

instance Tweaker ResponseTweaker where
  type Pattern ResponseTweaker = Response Action
  diff (ResponseTweaker ws) = responseDiff ws
  adjust _ = makeResponseSimilar

-- | @'responseDiff' x y@ compares the response patterns
--   @x@ and @y@, and returns a number between 0 and 1, representing
--   how different the patterns are. A result of 0 indicates that the
--   patterns are identical, apart from the outcome.
responseDiff
  :: Weights -> Response Action -> Response Action -> Difference
responseDiff ws x y = weightedSum ws [d1, d2]
  where d1 = actionDiff (_action x) (_action y)
        d2 = 1 - labelSimilarity (_labels x) (_labels y)

-- | @'makeResponseSimilar' target r pattern@ returns a modified copy
--   of @pattern@ that is more similar to @target@ than @pattern@ is.
--   The magnitude of the adjustment is controlled by the @r@
--   parameter, which should be a number between 0 and 1. Larger
--   values for @r@ permit greater adjustments. If @r@=1,
--   the result should be identical to the @target@. If @r@=0,
--   the result should be the unmodified @pattern@.
makeResponseSimilar
  :: Response Action -> UIDouble -> Response Action -> Response Action
makeResponseSimilar target r x = Response s a o
  where s = _labels x -- never change this
        a = makeActionSimilar (_action target) r (_action x)
        o = adjustPM1Vector (_outcomes target) r (_outcomes x)
