------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Prediction.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Do a full analysis of a wain and generate a report.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Wain.UIVector.Prediction.Experiment
import ALife.Creatur.Wain.UIVector.ExamineAgent (fetchWains, examine)
import System.Environment

main :: IO ()
main = do
  (f:_) <- getArgs
  ws <- fetchWains f :: IO [PatternWain]
  mapM_ examine ws
