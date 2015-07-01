------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.Wain
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.Prediction.Wain
  (
    PredictorWain,
    randomPredictorWain,
    VectorThinker(..),
    run,
    startRound,
    finishRound,
    schemaQuality,
    printStats,
    idealPopControlDeltaE -- exported for testing only
  ) where

import ALife.Creatur (agentId, isAlive)
import ALife.Creatur.Task (checkPopSize)
import ALife.Creatur.Wain (Wain, Label, buildWainAndGenerateGenome,
  name, chooseAction, incAge, applyMetabolismCost, condition,
  weanMatureChildren, pruneDeadChildren, adjustEnergy, adjustPassion,
  reflect, mate, litter, brain, energy, childEnergy, age, wainSize)
import ALife.Creatur.Wain.Brain (decider, Brain(..))
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import ALife.Creatur.Wain.Classifier(buildClassifier)
import ALife.Creatur.Wain.Decider(buildDecider)
import ALife.Creatur.Wain.GeneticSOM (RandomExponentialParams(..),
  randomExponential, numModels, schemaQuality, toList)
import ALife.Creatur.Wain.PlusMinusOne (pm1ToDouble)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, randomResponse, action,
  outcome, scenario)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (unitInterval, inRange, enforceRange)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.Prediction.Action (Action(..), predict)
import ALife.Creatur.Wain.Prediction.DataGen (nextVector)
import ALife.Creatur.Wain.Prediction.VectorThinker (VectorThinker(..))
import qualified ALife.Creatur.Wain.Prediction.Universe as U
import ALife.Creatur.Persistent (getPS, putPS)
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import qualified ALife.Creatur.Wain.Scenario as Sc
import ALife.Creatur.Wain.Statistics (summarise)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Conditional (whenM)
import Control.Lens hiding (universe)
import Control.Monad (replicateM, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR, getRandomRs,
  getRandom, getRandoms, evalRandIO)
import Control.Monad.State.Lazy (StateT, execStateT, get, put)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import Text.Printf (printf)

type PredictorWain = Wain [UIDouble] VectorThinker  Action

randomPredictorWain
  :: RandomGen r
    => String -> U.Universe PredictorWain -> Word16 -> Word16
      -> Rand r PredictorWain
randomPredictorWain wainName u classifierSize deciderSize = do
  let k = view U.uVectorLength u
  classifierModels <- replicateM (fromIntegral classifierSize)
    (fmap (take k) getRandoms)
  let fcp = RandomExponentialParams
               { _r0Range = view U.uClassifierR0Range u,
                 _dRange = view U.uClassifierDRange u }
  fc <- randomExponential fcp
  ws <- (makeWeights . take k) <$> getRandomRs unitInterval
  let c = buildClassifier fc (VectorThinker ws) classifierModels
  let fdp = RandomExponentialParams
              { _r0Range = view U.uDeciderR0Range u,
                _dRange = view U.uDeciderDRange u }
  fd <- randomExponential fdp
  xs <- replicateM (fromIntegral deciderSize) $
          randomResponse 1 (numModels c) 3 (view U.uOutcomeRange u)
  cw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  sw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  rw <- (makeWeights . take 2) <$> getRandomRs unitInterval
  let dr = buildDecider fd cw sw rw xs
  hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  let b = Brain c dr hw
  dv <- getRandomR . view U.uDevotionRange $ u
  m <- getRandomR . view U.uMaturityRange $ u
  p <- getRandomR unitInterval
  let app = replicate k $ doubleToUI 0
  return $ buildWainAndGenerateGenome wainName app b dv m p

data Summary = Summary
  {
    _rPopSize :: Int,
    _rVectorNovelty :: UIDouble,
    _rVectorAdjustedNovelty :: Int,
    _rPredDeltaE :: Double,
    _rChildPredDeltaE :: Double,
    _rMetabolismDeltaE :: Double,
    _rChildMetabolismDeltaE :: Double,
    _rPopControlDeltaE :: Double,
    _rChildPopControlDeltaE :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rOldAgeDeltaE :: Double,
    _rChildOldAgeDeltaE :: Double,
    _rPartnerMatingDeltaE :: Double,
    _rNetDeltaE :: Double,
    _rChildNetDeltaE :: Double,
    _rValuePredictionErr :: Double,
    _rRewardPredictionErr :: Double,
    _rBirthCount :: Int,
    _rWeanCount :: Int,
    _rFlirtCount :: Int,
    _rMateCount :: Int,
    _rDeathCount :: Int
  }
makeLenses ''Summary

initSummary :: Int -> Summary
initSummary p = Summary
  {
    _rPopSize = p,
    _rVectorNovelty = 0,
    _rVectorAdjustedNovelty = 0,
    _rPredDeltaE = 0,
    _rChildPredDeltaE = 0,
    _rMetabolismDeltaE = 0,
    _rChildMetabolismDeltaE = 0,
    _rPopControlDeltaE = 0,
    _rChildPopControlDeltaE = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOldAgeDeltaE = 0,
    _rChildOldAgeDeltaE = 0,
    _rPartnerMatingDeltaE = 0,
    _rNetDeltaE = 0,
    _rChildNetDeltaE = 0,
    _rValuePredictionErr = 0,
    _rRewardPredictionErr = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rDeathCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.iStat "pop. size" (view rPopSize r),
    Stats.dStat "DO novelty" (view rVectorNovelty r),
    Stats.iStat "DO novelty (adj.)"
      (view rVectorAdjustedNovelty r),
    Stats.dStat "adult prediction Δe" (view rPredDeltaE r),
    Stats.dStat "child prediction Δe" (view rChildPredDeltaE r),
    Stats.dStat "adult metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "child metabolism Δe" (view rChildMetabolismDeltaE r),
    Stats.dStat "adult pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "child pop. control Δe" (view rChildPopControlDeltaE r),
    Stats.dStat "adult flirting Δe" (view rFlirtingDeltaE r),
    Stats.dStat "adult mating Δe" (view rMatingDeltaE r),
    Stats.dStat "adult old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "child old age Δe" (view rChildOldAgeDeltaE r),
    Stats.dStat "other adult mating Δe" (view rPartnerMatingDeltaE r),
    Stats.dStat "adult net Δe" (view rNetDeltaE r),
    Stats.dStat "child net Δe" (view rChildNetDeltaE r),
    Stats.dStat "value pred. err" (view rValuePredictionErr r),
    Stats.dStat "reward pred. err" (view rRewardPredictionErr r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "died" (view rDeathCount r)
  ]

data Experiment = Experiment
  {
    _subject :: PredictorWain,
    _partner :: PredictorWain,
    _weanlings :: [PredictorWain],
    _universe :: U.Universe PredictorWain,
    _summary :: Summary
  }
makeLenses ''Experiment

scaledDelta :: UIDouble -> UIDouble -> UIDouble
scaledDelta x y = doubleToUI $ (uiToDouble x - uiToDouble y)/2 + 0.5

startRound :: StateT (U.Universe PredictorWain) IO ()
startRound = do
  U.writeToLog $ "DEBUG 1"
  xsOld <- zoom U.uCurrVector getPS
  U.writeToLog $ "DEBUG 2"
  xs <- nextVector
  U.writeToLog $ "DEBUG 3"
  let deltas = zipWith scaledDelta xs xsOld
    -- xs is shorter because it doesn't include any deltas, so the
    -- result will be the same length as xs, and won't include any
    -- deltas of previous deltas.
  U.writeToLog $ "DEBUG 4"
  zoom U.uCurrVector $ putPS (xs ++ deltas)
  U.writeToLog $ "Current data: " ++ show xs
  U.writeToLog $ "Deltas: " ++ show deltas
  let actual = head xs
  ps <- zoom U.uPredictions getPS
  when (not . null $ ps) $ do
    let predicted = mean . map uiToDouble $ thirdOfThree ps
    let err = abs (uiToDouble actual - predicted)
    U.writeToLog $ "actual=" ++ show actual
      ++ " predicted=" ++ show predicted
      ++ " err=" ++ show err
  margin <- use U.uAccuracyMargin
  let a = doubleToUI . enforceRange unitInterval $
            (uiToDouble actual - uiToDouble margin)
  let b = doubleToUI . enforceRange unitInterval $
            (uiToDouble actual + uiToDouble margin)
  zoom U.uCurrentAccuracyRange $ putPS (a,b)
  U.writeToLog $ "margins=" ++ show (a, b)

finishRound :: StateT (U.Universe PredictorWain) IO ()
finishRound = do
  v <- use U.uCurrVector
  assign U.uPrevVector v
  f <- use U.uStatsFile
  xss <- readStats f
  let yss = summarise xss
  printStats yss
  let zs = concat yss
  adjustPopControlDeltaE zs
  cs <- use U.uCheckpoints
  enforceAll zs cs
  clearStats f
  (a, b) <- use U.uPopulationAllowedRange
  checkPopSize (a, b)

run :: [PredictorWain]
      -> StateT (U.Universe PredictorWain) IO [PredictorWain]
run (me:other:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last two wains  standing!"
  p <- U.popSize
  u <- get
  let e = Experiment { _subject = me,
                       _partner = other,
                       _weanlings = [],
                       _universe = u,
                       _summary = initSummary p}
  e' <- liftIO $ execStateT run' e
  put (view universe e')
  let modifiedAgents
        = view subject e' : view partner e' : view weanlings e'
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  return modifiedAgents
run _ = error "no more wains"

run' :: StateT Experiment IO ()
run' = do
  (e0, ec0) <- totalEnergy
  a <- use subject
  zoom universe . U.writeToLog $ "---------- " ++ agentId a
    ++ "'s turn ----------"
  zoom universe . U.writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  rewardPrediction
  runMetabolism
  killIfTooOld
  applyPopControl
  adjustSubjectPassion
  subject %= incAge
  maybeFlirt
  makePrediction
  a' <- use subject
  zoom universe . U.writeToLog $ "End of " ++ agentId a ++ "'s turn"
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  agentStats <- ((Stats.stats a' ++) . summaryStats) <$> use summary
  zoom universe . U.writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  rsf <- use (universe . U.uRawStatsFile)
  zoom universe $ writeRawStats (agentId a) rsf agentStats
  -- whenM (use (universe . U.uGenFmris)) writeFmri
  sf <- use (universe . U.uStatsFile)
  zoom universe $ updateStats agentStats sf

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetDeltaE = _rPredDeltaE s
         + _rMetabolismDeltaE s
         + _rPopControlDeltaE s
         + _rFlirtingDeltaE s
         + _rMatingDeltaE s
         + _rPartnerMatingDeltaE s
         + _rOldAgeDeltaE s, 
    _rChildNetDeltaE = _rChildPredDeltaE s
         + _rChildMetabolismDeltaE s
         + _rChildPopControlDeltaE s
         + _rChildOldAgeDeltaE s
         -- include energy given to wains when they are born
         - _rMatingDeltaE s
         - _rPartnerMatingDeltaE s
  }

balanceEnergyEquation
  :: Double -> Double -> Double -> Double -> StateT Experiment IO ()
balanceEnergyEquation e0 ec0 ef ecf = do
  netDeltaE1 <- use (summary . rNetDeltaE)
  let netDeltaE2 = ef - e0
  let err = abs (netDeltaE1 - netDeltaE2)
  when (err > 0.000001) $ do
    zoom universe . U.writeToLog $
      "WARNING: Adult energy equation doesn't balance"
    zoom universe . U.writeToLog $
      "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.000001) $ do
    zoom universe . U.writeToLog $
      "WARNING: Child energy equation doesn't balance"
    zoom universe . U.writeToLog $ "ec0=" ++ show ec0
      ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bms <- use (universe . U.uBaseMetabolismDeltaE)
  cps <- use (universe . U.uEnergyCostPerByte)
  ccf <- use (universe . U.uChildCostFactor)
  let (a', adultCost, childCost) = applyMetabolismCost bms cps ccf a
  zoom universe . U.writeToLog $
    "bms=" ++ show bms ++ " cps=" ++ show cps ++ " adult size="
    ++ show (view wainSize a) ++ " adult cost=" ++ show adultCost
  (summary . rMetabolismDeltaE) += adultCost
  (summary . rChildMetabolismDeltaE) += childCost
  assign subject a'

rewardPrediction :: StateT Experiment IO ()
rewardPrediction = do
  a <- use subject
  ps <- zoom (universe . U.uPredictions) getPS
  case lookup3 (agentId a) ps of
    Nothing ->
      zoom universe . U.writeToLog $ "First turn for " ++ agentId a
    Just (r, predicted) -> do
      -- The wain's condition may have changed between the time it
      -- made the decision and the time it gets the reward (e.g., as
      -- a result of mating).
      let r' = set (scenario . Sc.condition) (condition a) r
      range <- zoom (universe . U.uCurrentAccuracyRange) getPS
      accuracyDeltaE <- use (universe . U.uAccuracyDeltaE)
      let deltaE = if inRange range predicted then accuracyDeltaE else 0
      actual <- head <$> zoom (universe . U.uCurrVector) getPS
      zoom universe . U.writeToLog $
        agentId a ++ " predicted " ++ show predicted
        ++ ", actual value was " ++ show actual
        ++ ", reward is " ++ show deltaE
      let err = abs $
                  uiToDouble actual - uiToDouble predicted
      assign (summary . rValuePredictionErr) err
      adjustWainEnergy subject deltaE rPredDeltaE rChildPredDeltaE
      letSubjectReflect r'
      zoom (universe . U.uPredictions) . putPS . remove3 (agentId a) $ ps

-- calculateRewards :: StateT (U.Universe PredictorWain) IO ()
-- calculateRewards = do
--   (x:_) <- zoom U.uCurrVector getPS
--   ps <- zoom U.uPredictions getPS
--   U.writeToLog $ "Predictions " ++ show (map dropMiddleOf3 ps)
--   deltaE <- use U.uAccuracyDeltaE
--   let rs = map (calculateReward range deltaE) ps
--   U.writeToLog $ "Rewards " ++ show (map dropMiddleOf4 rs)
--   zoom U.uRewards (putPS rs)
--   mapM_ (U.writeToLog . describeReward x) rs

-- calculateReward
--   :: (UIDouble, UIDouble) -> Double
--     -> (AgentId, Response Action, UIDouble)
--       -> (AgentId, Response Action, UIDouble, Double)
-- calculateReward (a, b) deltaE (w, r, pred) =
--   if a <= pred && pred <= b
--     then (w, r, pred, deltaE)
--     else (w, r, pred, 0)

-- dropMiddleOf3 :: (a, b, c) -> (a, c)
-- dropMiddleOf3 (a, _, c) = (a, c)

-- dropMiddleOf4 :: (a, b, c, d) -> (a, d)
-- dropMiddleOf4 (a, _, _, d) = (a, d)

-- fourthOf4 :: (a, b, c, d) -> d
-- fourthOf4 (_, _, _, d) = d

-- describeReward
--   :: UIDouble -> (AgentId, Response Action, UIDouble, Double) -> String
-- describeReward actual (a, _, predicted, deltaE)
--   = a ++ " predicted " ++ show predicted ++ ", actual value was "
--       ++ show actual ++ ", reward is " ++ show deltaE

-- calculateError
--   :: UIDouble -> (AgentId, Response Action, UIDouble)
--     -> (AgentId, Response Action, UIDouble, Double)
-- calculateError x (a, r, p)
--   = (a, r, p, abs(uiToDouble x - uiToDouble p))

lookup3 :: Eq a => a -> [(a, b, c)] -> Maybe (b, c)
lookup3 k ((a, b, c):xs) | a == k    = Just (b, c)
                         | otherwise = lookup3 k xs
lookup3 _ [] = Nothing

remove3 :: Eq a => a -> [(a, b, c)] -> [(a, b, c)]
remove3 k ((a, b, c):xs) | a == k    = xs
                         | otherwise = (a, b, c):remove3 k xs
remove3 _ [] = []

thirdOfThree :: [(a, b, c)] -> [c]
thirdOfThree ((_, _, c):xs) = c : thirdOfThree xs
thirdOfThree [] = []

makePrediction :: StateT Experiment IO ()
makePrediction = do
  a <- use subject
  dObj <- zoom (universe . U.uCurrVector) getPS
  (dObjNovelty, dObjNoveltyAdj, r, a') 
    <- zoom universe $ chooseAction3 a dObj
  assign (summary.rVectorNovelty) dObjNovelty
  assign (summary.rVectorAdjustedNovelty) dObjNoveltyAdj
  assign subject a'
  ps <- zoom (universe . U.uPredictions) getPS
  when (null dObj) $
    zoom universe . U.writeToLog $ "WARNING: dObj is null"
  let x = head dObj
  let xPredicted = predict (view action r) x
  zoom universe . U.writeToLog $
    agentId a ++ " predicts " ++ show xPredicted
  let ps' = (agentId a, r, xPredicted) : ps
  zoom (universe . U.uPredictions) $ putPS ps'

chooseAction3
  :: PredictorWain -> [UIDouble]
    -> StateT (U.Universe PredictorWain) IO
        (UIDouble, Int, Response Action, PredictorWain)
chooseAction3 w vs = do
  whenM (use U.uShowDeciderModels) $ describeModels w
  let (cl:_, sl, r, w', xs, dObjNovelty:_) = chooseAction [vs] w
  whenM (use U.uShowPredictions) $ describeOutcomes w xs
  let dObjNoveltyAdj = round . uiToDouble $
        dObjNovelty * fromIntegral (view age w)
  U.writeToLog $ "To " ++ agentId w ++ ", " ++ show vs
    ++ " has adjusted novelty " ++ show dObjNoveltyAdj
    ++ " and best fits classifier model " ++ show cl
  U.writeToLog $ agentId w ++ " sees " ++ show vs
    ++ " and chooses " ++ show (view action r)
    ++ " based on response model " ++ show sl
  return (dObjNovelty, dObjNoveltyAdj, r, w')

describeModels
  :: PredictorWain -> StateT (U.Universe PredictorWain) IO ()
describeModels w = mapM_ (U.writeToLog . f) ms
  where ms = toList . view decider $ view brain w
        f (l, r) = view name w ++ "'s decider model " ++ show l ++ "="
                     ++ pretty r

describeOutcomes
  :: PredictorWain -> [(Response Action, Label)]
    -> StateT (U.Universe PredictorWain) IO ()
describeOutcomes w = mapM_ (U.writeToLog . f)
  where f (r, l) = view name w ++ "'s predicted outcome of "
                     ++ show (view action r) ++ " is "
                     ++ (printf "%.3f" . pm1ToDouble . fromJust .
                          view outcome $ r)
                     ++ " from model " ++ show l

--
-- Utility functions
--

applyPopControl :: StateT Experiment IO ()
applyPopControl = do
  deltaE <- zoom (universe . U.uPopControlDeltaE) getPS
  adjustWainEnergy subject deltaE rPopControlDeltaE
    rChildPopControlDeltaE

maybeFlirt :: StateT Experiment IO ()
maybeFlirt = do
  frequency <- use (universe . U.uFlirtingFrequency)
  x <- getRandom
  when (x < frequency) flirt

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  b <- use partner
  babyName <- zoom universe U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ mate a b babyName
  if null msgs
    then do
      assign subject a'
      assign partner b'
      (summary.rBirthCount) += length (view litter a)
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rPartnerMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ (zoom universe . U.writeToLog) msgs

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- weanMatureChildren <$> use subject
  assign subject a
  (a':deadChildren) <- pruneDeadChildren <$> use subject
  assign subject a'
  assign weanlings (matureChildren ++ deadChildren)
  (summary.rWeanCount) += length matureChildren

killIfTooOld :: StateT Experiment IO ()
killIfTooOld = do
  a <- view age <$> use subject
  maxAge <- use (universe . U.uMaxAge)
  when (fromIntegral a > maxAge) $
    adjustWainEnergy subject (-1) rOldAgeDeltaE
      rChildOldAgeDeltaE

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe PredictorWain) IO ()
adjustPopControlDeltaE xs =
  unless (null xs) $ do
    pop <- U.popSize
    U.writeToLog $ "pop=" ++ show pop
    idealPop <- use U.uIdealPopulationSize
    U.writeToLog $ "ideal pop=" ++ show idealPop

    let (Just adultNet) = Stats.lookup "avg. adult net Δe" xs
    U.writeToLog $ "adultNet=" ++ show adultNet
    let (Just childNet) = Stats.lookup "avg. child net Δe" xs
    U.writeToLog $ "childNet=" ++ show childNet

    let (Just adultPopControl)
          = Stats.lookup "avg. adult pop. control Δe" xs
    U.writeToLog $ "adultPopControl=" ++ show adultPopControl
    let (Just childPopControl)
          = Stats.lookup "avg. child pop. control Δe" xs
    U.writeToLog $ "childPopControl=" ++ show childPopControl

    let avgEnergyToBalance 
          = adultNet + childNet - adultPopControl - childPopControl
    U.writeToLog $ "avgEnergyToBalance=" ++ show avgEnergyToBalance
    let c = idealPopControlDeltaE idealPop pop avgEnergyToBalance
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

idealPopControlDeltaE :: Int -> Int -> Double -> Double
idealPopControlDeltaE idealPop pop e
  | idealPop == 0 = error "idealPop == 0"
  | pop == 0      = error "pop == 0"
  | otherwise    = -f*e
  where f = if e < 0
              then fromIntegral idealPop / fromIntegral pop
              else fromIntegral pop / fromIntegral idealPop

-- lookupStat
--   :: String -> [Stats.Statistic]
--     -> StateT (U.Universe PredictorWain) IO (Maybe Double)
-- lookupStat key xs = do
--   let result = Stats.lookup key xs
--   when (isNothing result && not (null xs)) $ -- ignore missing stats file
--     requestShutdown $ "Cannot find statistic: " ++ key
--   return result

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- view energy <$> use subject
  b <- view energy <$> use partner
  c <- childEnergy <$> use subject
  d <- childEnergy <$> use partner
  return (uiToDouble a + uiToDouble b, uiToDouble c + uiToDouble d)

printStats
  :: [[Stats.Statistic]] -> StateT (U.Universe PredictorWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

adjustWainEnergy
  :: Simple Lens Experiment PredictorWain -> Double
    -> Simple Lens Summary Double -> Simple Lens Summary Double
    -> StateT Experiment IO ()
adjustWainEnergy wainSelector deltaE adultEnergySelector
    childEnergySelector = do
  x <- use wainSelector
  let (x', adultDeltaE, childDeltaE)
        = adjustEnergy deltaE x
  (summary . adultEnergySelector) += adultDeltaE
  when (childDeltaE /= 0) $
    (summary . childEnergySelector) += childDeltaE
  assign subject x'

-- Increases passion (cooling down happens automatically when mating)
adjustSubjectPassion
  :: StateT Experiment IO ()
adjustSubjectPassion = subject %= adjustPassion

letSubjectReflect
  :: Response Action -> StateT Experiment IO ()
letSubjectReflect r = do
  x <- use subject
  p <- zoom (universe . U.uPrevVector) getPS
  let (x', err) = reflect [p] r x
  assign subject x'
  assign (summary . rRewardPredictionErr) err

writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe PredictorWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"

-- mean :: (Eq a, Fractional a, Foldable t) => t a -> a
mean :: (Fractional a, Eq a) => [a] -> a
mean xs
  | count == 0 = error "no data"
  | otherwise = total / count
  where (total, count) = foldr f (0, 0) xs
        f x (y, n) = (y+x, n+1)
