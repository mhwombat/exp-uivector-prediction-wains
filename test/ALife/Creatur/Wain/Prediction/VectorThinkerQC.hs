------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.VectorThinkerQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Prediction.VectorThinkerQC
  (
    test
  ) where

import ALife.Creatur.Wain.WeightsInternal (Weights, makeWeights,
  toUIDoubles)
import ALife.Creatur.Wain.Prediction.VectorThinker
import ALife.Creatur.Wain.Prediction.TestUtils
  (prop_serialize_round_trippable, prop_genetic_round_trippable,
  prop_diploid_identity)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (unitInterval)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary UIDouble where
  arbitrary = doubleToUI <$> choose unitInterval

sizedArbWeights :: Int -> Gen Weights
sizedArbWeights n = fmap makeWeights . vectorOf n $ choose unitInterval

instance Arbitrary Weights where
  arbitrary = sized sizedArbWeights

equivWeights :: Weights -> Weights -> Bool
equivWeights x y = and $ zipWith f (map uiToDouble $ toUIDoubles x)
                                   (map uiToDouble $ toUIDoubles y)
  where f a b = abs (a - b) <= 1/255


instance Arbitrary VectorThinker where
  arbitrary = VectorThinker <$> arbitrary

equiv :: VectorThinker -> VectorThinker -> Bool
equiv (VectorThinker a) (VectorThinker b) = equivWeights a b

test :: Test
test = testGroup "ALife.Creatur.Wain.Prediction.VectorThinkerQC"
  [
    testProperty "prop_serialize_round_trippable - VectorThinker"
      (prop_serialize_round_trippable :: VectorThinker -> Property),
    testProperty "prop_genetic_round_trippable - VectorThinker"
      (prop_genetic_round_trippable equiv :: VectorThinker -> Property),
    testProperty "prop_diploid_identity - VectorThinker"
      (prop_diploid_identity (==) :: VectorThinker -> Property)
  ]
