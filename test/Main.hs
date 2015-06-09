------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Wain.Prediction.ActionQC (test)
import ALife.Creatur.Wain.Prediction.VectorThinkerQC (test)
import ALife.Creatur.Wain.Prediction.WainQC (test)

import Test.Framework as TF (defaultMain, Test)

tests :: [TF.Test]
tests = 
  [
    -- In increasing order of complexity
    ALife.Creatur.Wain.Prediction.ActionQC.test,
    ALife.Creatur.Wain.Prediction.VectorThinkerQC.test,
    ALife.Creatur.Wain.Prediction.WainQC.test
  ]

main :: IO ()
main = defaultMain tests
