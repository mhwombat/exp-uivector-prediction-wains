------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Prediction.ActionQC
-- Copyright   :  (c) Amy de Buitléir 2013-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.UIVector.Prediction.ActionQC
  (
    test
  ) where

import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import ALife.Creatur.Wain.UIVector.Prediction.Action
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Action where
  arbitrary = elements [minBound .. maxBound]

prop_action_diff_is_reflexive :: Action -> Action -> Property
prop_action_diff_is_reflexive a b
  = property $ actionDiff a b == actionDiff b a

prop_action_diff_can_be_zero :: Action -> Property
prop_action_diff_can_be_zero a = property $ actionDiff a a == 0

prop_action_diff_can_be_one :: Property
prop_action_diff_can_be_one
  = property $ actionDiff minBound maxBound == 1

prop_predict_consistent_with_postdict :: UIDouble -> Action -> Property
prop_predict_consistent_with_postdict x a =
 0 < xa && xa < 1 ==> abs (uiToDouble xa - uiToDouble xb) < 0.01
 where xa = predict a x
       a' = postdict x xa
       xb = predict a' x

test :: Test
test = testGroup "ALife.Creatur.Wain.UIVector.Prediction.ActionQC"
  [
    testProperty "prop_serialize_round_trippable - Action"
      (prop_serialize_round_trippable :: Action -> Property),
    testProperty "prop_genetic_round_trippable - Action"
      (prop_genetic_round_trippable (==) :: Action -> Property),
    testProperty "prop_diploid_identity - Action"
      (prop_diploid_identity (==) :: Action -> Property),
    testProperty "prop_action_diff_is_reflexive"
      prop_action_diff_is_reflexive,
    testProperty "prop_action_diff_can_be_zero"
      prop_action_diff_can_be_zero,
    testProperty "prop_action_diff_can_be_one"
      prop_action_diff_can_be_one,
    testProperty "prop_predict_consistent_with_postdict"
      prop_predict_consistent_with_postdict
  ]
