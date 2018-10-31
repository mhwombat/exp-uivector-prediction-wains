------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2013-2018
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
import ALife.Creatur.Wain.ExamineAgent (fetchObjects, examine)
import System.Environment

main :: IO ()
main = do
  (f:_) <- getArgs
  ws <- fetchObjects f :: IO [PatternWain]
  mapM_ examine ws
