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

import ALife.Creatur (agentId)
import ALife.Creatur.Wain (Wain, brain)
import ALife.Creatur.Wain.Brain (classifier, predictor)
import ALife.Creatur.Wain.ExamineAgent (fetchObjects)
import ALife.Creatur.Wain.GeneticSOM (Label, modelMap, counterMap, tweaker)
import ALife.Creatur.Wain.PlusMinusOne (pm1ToDouble)
import ALife.Creatur.Wain.Pretty (Pretty(..))
import ALife.Creatur.Wain.Response (Response, labels, action, outcomes)
import ALife.Creatur.Wain.UIVector.Prediction.Experiment
import Control.Lens (view)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import System.Environment
import Text.Printf (printf)

main :: IO ()
main = do
  (f:_) <- getArgs
  ws <- fetchObjects f :: IO [PatternWain]
  mapM_ report ws

report
  :: (Pretty p, Pretty a, Pretty ct, Pretty pt)
    => Wain p ct pt m a -> IO ()
report a = do
  putStrLn $ "Classifier tweaker: " ++ pretty (view tweaker . view classifier . view brain $ a)
  putStrLn $ "Predictor counts: " ++ pretty (counterMap . view predictor . view brain $ a)
  mapM_ putStrLn $ describePredictorModels a

describePredictorModels
  :: (Pretty p, Pretty a)
    => Wain p ct pt m a -> [String]
describePredictorModels w = map f rs
  where rs = M.toList . modelMap . view (brain . predictor) $ w
        f (l, r) = agentId w ++ "'s predictor model "
                     ++ pretty l ++ ": " ++ (describePredictorModel r cMap)
        cMap = modelMap . view (brain . classifier) $ w

describePredictorModel
  :: (Pretty p, Pretty a)
    => Response a -> M.Map Label p -> String
describePredictorModel r cMap =
  intercalate "," (map (\l -> pretty (forceLookup l cMap)) ls) ++ '|':pretty a ++ '|':format os
    where ls = view labels r
          a = view action r
          os = view outcomes r
          format xs =  intercalate "|" . map (printf "%.3f" .  pm1ToDouble) $ xs

forceLookup :: Ord p => p -> M.Map p a -> a
forceLookup k m = v
  where (Just v) = M.lookup k m
