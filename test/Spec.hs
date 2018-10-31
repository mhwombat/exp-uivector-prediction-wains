------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2012-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
import ALife.Creatur.Wain.UIVector.Prediction.ActionQC (test)
import ALife.Creatur.Wain.UIVector.Prediction.ExperimentQC (test)

import Test.Framework as TF (defaultMain, Test)

tests :: [TF.Test]
tests = 
  [
    -- In increasing order of complexity
    ALife.Creatur.Wain.UIVector.Prediction.ActionQC.test,
    ALife.Creatur.Wain.UIVector.Prediction.ExperimentQC.test
  ]

main :: IO ()
main = defaultMain tests
