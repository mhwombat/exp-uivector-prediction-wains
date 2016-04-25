------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Prediction.Experiment
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
module ALife.Creatur.Wain.UIVector.Prediction.Experiment
  (
    PatternWain,
    randomPatternWain,
    run,
    startRound,
    finishRound,
    schemaQuality,
    printStats,
    versionInfo,
    idealPopControlDeltaE -- exported for testing only
  ) where

import ALife.Creatur (agentId, isAlive, programVersion)
import ALife.Creatur.Persistent (getPS, putPS)
import ALife.Creatur.Task (checkPopSize)
import qualified ALife.Creatur.Wain as W
import ALife.Creatur.Wain.Brain (makeBrain, scenarioReport,
  responseReport, decisionReport, predictor, classifier)
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.Muser (makeMuser)
import qualified ALife.Creatur.Wain.Predictor as P
import ALife.Creatur.Wain.GeneticSOM (RandomLearningParams(..),
  randomLearningFunction, schemaQuality, modelMap, currentLearningRate,
  numModels)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.UIVector.Prediction.Action (Action(..), predict,
  numActions)
import ALife.Creatur.Wain.UIVector.Prediction.DataSource (nextVector)
import ALife.Creatur.Wain.UIVector.Tweaker (PatternTweaker(..))
import qualified ALife.Creatur.Wain.UIVector.Prediction.Universe as U
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, action, outcomes)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Wain.Statistics (summarise)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble,
  doubleToUI)
import ALife.Creatur.Wain.Util (unitInterval, enforceRange, inRange)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Conditional (whenM)
import Control.Lens hiding (universe)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandomR,
  getRandomRs, getRandoms, evalRandIO)
import Control.Monad.State.Lazy (StateT, execStateT, get, put)
import Data.List (intercalate, minimumBy)
import Data.Map (toList)
import Data.Ord (comparing)
import Data.Version (showVersion)
import Data.Word (Word64)
import Paths_exp_uivector_prediction_wains (version)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import Text.Printf (printf)

versionInfo :: String
versionInfo
  = "exp-prediction-wains-" ++ showVersion version
      ++ ", compiled with " ++ W.packageVersion
      ++ ", " ++ ALife.Creatur.programVersion

type PatternWain = W.Wain [UIDouble] PatternTweaker  Action

randomPatternWain
  :: RandomGen r
    => String -> U.Universe PatternWain -> Word64
      -> Rand r PatternWain
randomPatternWain wName u classifierSize = do
  let k = view U.uVectorLength u
  let fcp = RandomLearningParams
              { _r0Range = view U.uPredictorR0Range u,
                _rfRange = view U.uPredictorRfRange u,
                _tfRange = view U.uPredictorTfRange u }
  fc <- randomLearningFunction fcp
  classifierThreshold <- getRandomR (view U.uClassifierThresholdRange u)
  ws <- (makeWeights . take k) <$> getRandoms
  let c = Cl.buildClassifier fc classifierSize classifierThreshold
            (PatternTweaker ws)
  let fdp = RandomLearningParams
              { _r0Range = view U.uPredictorR0Range u,
                _rfRange = view U.uPredictorRfRange u,
                _tfRange = view U.uPredictorTfRange u }
  fd <- randomLearningFunction fdp
  predictorThreshold <- getRandomR (view U.uPredictorThresholdRange u)
  let predictorSize = classifierSize * fromIntegral numActions
  let dr = P.buildPredictor fd predictorSize predictorThreshold
  hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  dOut <- take 4 <$> getRandomRs (view U.uDefaultOutcomeRange u)
  dp <- getRandomR $ view U.uDepthRange u
  let mr = makeMuser dOut dp
  t <- getRandom
  ios <- take 4 <$> getRandomRs (view U.uImprintOutcomeRange u)
  rds <- take 4 <$> getRandomRs (view U.uReinforcementDeltasRange u)
  let (Right wBrain) = makeBrain c mr dr hw t ios rds
  wDevotion <- getRandomR . view U.uDevotionRange $ u
  wAgeOfMaturity <- getRandomR . view U.uMaturityRange $ u
  let wPassionDelta = 0
  let wBoredomDelta = 0
  let wAppearance = replicate k $ doubleToUI 0
  return $ W.buildWainAndGenerateGenome wName wAppearance wBrain
    wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta

data Summary = Summary
  {
    _rPopSize :: Int,
    _rVectorNovelty :: UIDouble,
    _rVectorAdjustedNovelty :: Int,
    _rPredDeltaE :: Double,
    _rMetabolismDeltaE :: Double,
    _rPopControlDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rOldAgeDeltaE :: Double,
    _rOtherMatingDeltaE :: Double,
    _rNetDeltaE :: Double,
    _rChildNetDeltaE :: Double,
    _rPredictedValue :: UIDouble,
    _rActualValue :: UIDouble,
    _rValuePredictionErr :: Double,
    _rRewardPredictionErr :: Double,
    _rRewardCount :: Int,
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
    _rMetabolismDeltaE = 0,
    _rPopControlDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOldAgeDeltaE = 0,
    _rOtherMatingDeltaE = 0,
    _rNetDeltaE = 0,
    _rChildNetDeltaE = 0,
    _rPredictedValue = 0,
    _rActualValue = 0,
    _rValuePredictionErr = 0,
    _rRewardPredictionErr = 0,
    _rRewardCount = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rDeathCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.dStat "pop. size" (view rPopSize r),
    Stats.dStat "DO novelty" (view rVectorNovelty r),
    Stats.iStat "DO novelty (adj.)"
      (view rVectorAdjustedNovelty r),
    Stats.dStat "adult prediction Δe" (view rPredDeltaE r),
    Stats.dStat "adult metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "adult pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "adult mating Δe" (view rMatingDeltaE r),
    Stats.dStat "adult old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "other adult mating Δe" (view rOtherMatingDeltaE r),
    Stats.dStat "adult net Δe" (view rNetDeltaE r),
    Stats.dStat "child net Δe" (view rChildNetDeltaE r),
    Stats.dStat "predicted value" (view rPredictedValue r),
    Stats.dStat "actual value" (view rActualValue r),
    Stats.dStat "value pred. err" (view rValuePredictionErr r),
    Stats.dStat "reward pred err" (view rRewardPredictionErr r),
    Stats.dStat "reward count" (view rRewardCount r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "died" (view rDeathCount r)
  ]

data Experiment = Experiment
  {
    _subject :: PatternWain,
    _other :: PatternWain,
    _weanlings :: [PatternWain],
    _universe :: U.Universe PatternWain,
    _summary :: Summary
  }
makeLenses ''Experiment

scaledDelta :: UIDouble -> UIDouble -> UIDouble
scaledDelta x y = doubleToUI $ (uiToDouble x - uiToDouble y)/2 + 0.5

startRound :: StateT (U.Universe PatternWain) IO ()
startRound = do
  U.writeToLog $ "DEBUG 1"
  xsOld <- zoom U.uCurrVector getPS
  U.writeToLog $ "DEBUG 2"
  xs <- zoom U.uDataSource nextVector
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
  zoom U.uCurrentAccuracyRange $ putPS (a, b)
  U.writeToLog $ "margins=" ++ show (a, b)

finishRound :: StateT (U.Universe PatternWain) IO ()
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
  (a, b) <- use U.uAllowedPopulationRange
  checkPopSize (a, b)

report :: String -> StateT Experiment IO ()
report = zoom universe . U.writeToLog

run :: [PatternWain]
      -> StateT (U.Universe PatternWain) IO [PatternWain]
run (me:otherWain:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last two wains  standing!"
  p <- U.popSize
  u <- get
  let e = Experiment { _subject = me,
                       _other = otherWain,
                       _weanlings = [],
                       _universe = u,
                       _summary = initSummary p}
  e' <- liftIO $ execStateT run' e
  put (view universe e')
  let modifiedAgents
        = view subject e' : view other e' : view weanlings e'
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  reportAnyDeaths modifiedAgents
  return modifiedAgents
run _ = error "too few wains"

run' :: StateT Experiment IO ()
run' = do
  (e0, ec0) <- totalEnergy
  a <- use subject
  report $ "---------- " ++ agentId a ++ "'s turn ----------"
  report $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  rewardPrediction
  runMetabolism
  autoPopControl <- use (universe . U.uPopControl)
  when autoPopControl applyPopControl
  subject %= W.incAge
  maybeFlirt
  makePrediction
  a' <- use subject
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  killIfTooOld
  agentStats <- ((Stats.stats a' ++) . summaryStats) <$> use summary
  report $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  rsf <- use (universe . U.uRawStatsFile)
  zoom universe $ writeRawStats (agentId a) rsf agentStats
  sf <- use (universe . U.uStatsFile)
  zoom universe $ updateStats agentStats sf

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetDeltaE = _rPredDeltaE s
         + _rMetabolismDeltaE s
         + _rPopControlDeltaE s
         + _rMatingDeltaE s
         + _rOldAgeDeltaE s
         + _rOtherMatingDeltaE s,
    _rChildNetDeltaE = 0
         -- include energy given to wains when they are born
         - _rMatingDeltaE s
         - _rOtherMatingDeltaE s
  }

balanceEnergyEquation
  :: Double -> Double -> Double -> Double -> StateT Experiment IO ()
balanceEnergyEquation e0 ec0 ef ecf = do
  netDeltaE1 <- use (summary . rNetDeltaE)
  let netDeltaE2 = ef - e0
  let err = abs (netDeltaE1 - netDeltaE2)
  when (err > 0.000001) $ do
    report $ "WARNING: Adult energy equation doesn't balance"
    report $ "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.000001) $ do
    report $ "WARNING: Child energy equation doesn't balance"
    report $ "ec0=" ++ show ec0 ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bmc <- use (universe . U.uBaseMetabolismDeltaE)
  cpcm <- use (universe . U.uEnergyCostPerClassifierModel)
  ccf <- use (universe . U.uChildCostFactor)
  let deltaE = metabCost bmc cpcm 1 a
                 + sum (map (metabCost bmc cpcm ccf) (view W.litter a))
  adjustWainEnergy subject deltaE rMetabolismDeltaE "metabolism"

metabCost :: Double -> Double -> Double -> PatternWain -> Double
metabCost bmc cpcm scale w = scale * (bmc + cpcm * fromIntegral n)
  where n = numModels . view (W.brain . classifier) $ w

rewardPrediction :: StateT Experiment IO ()
rewardPrediction = do
  a <- use subject
  ps <- zoom (universe . U.uPredictions) getPS
  case lookup3 (agentId a) ps of
    Nothing ->
      zoom universe . U.writeToLog $ "First turn for " ++ agentId a
    Just (r, predicted) -> do
      range <- zoom (universe . U.uCurrentAccuracyRange) getPS
      accuracyDeltaE <- use (universe . U.uAccuracyDeltaE)
      let deltaE = if inRange range predicted then accuracyDeltaE else 0
      adjustWainEnergy subject deltaE rPredDeltaE "prediction"
      actual <- head <$> zoom (universe . U.uCurrVector) getPS
      zoom universe . U.writeToLog $
        agentId a ++ " predicted " ++ show predicted
        ++ ", actual value was " ++ show actual
        ++ ", reward is " ++ show deltaE
      when (deltaE > 0) $ assign (summary . rRewardCount) 1
      assign (summary . rPredictedValue) predicted
      zoom universe . U.writeToLog $ "DEBUG 20a"
      assign (summary . rActualValue) actual
      zoom universe . U.writeToLog $ "DEBUG 20b"
      let err = abs $ uiToDouble actual - uiToDouble predicted
      zoom universe . U.writeToLog $ "DEBUG 20c err=" ++ show err
      assign (summary . rValuePredictionErr) err
      zoom universe . U.writeToLog $ "DEBUG 21"
      a' <- use subject
      let wombat = view (W.brain . predictor) a'
      zoom universe . U.writeToLog $ "DEBUG predictor learning rate=" ++ show (currentLearningRate wombat)
      letSubjectReflect a r
      zoom universe . U.writeToLog $ "DEBUG 23"
      zoom (universe . U.uPredictions) . putPS . remove3 (agentId a) $ ps
      zoom universe . U.writeToLog $ "DEBUG 24"
      a'' <- use subject
      let wombat' = view (W.brain . predictor) a''
      zoom universe . U.writeToLog $ "DEBUG predictor learning rate=" ++ show (currentLearningRate wombat')

chooseAction3
  :: PatternWain -> [UIDouble]
    -> StateT (U.Universe PatternWain) IO
        (UIDouble, Int, Response Action, PatternWain)
chooseAction3 w vs = do
  U.writeToLog $ "DEBUG 10"
  U.writeToLog $ "DEBUG 11 vs=" ++ show vs
  whenM (use U.uShowPredictorModels) $ describeModels w
  U.writeToLog $ "DEBUG 12"
  let (lds, sps, rplos, aohs, r, w')
        = W.chooseAction [vs] w
  let (_, dObjNovelty, dObjNoveltyAdj)
          = analyseClassification lds w
  U.writeToLog $ "DEBUG 13"
  whenM (use U.uShowPredictions) $ describeOutcomes w rplos
  U.writeToLog $ "DEBUG 14"
  U.writeToLog $ "DEBUG nov=" ++ show dObjNovelty ++ ", age=" ++ show (view W.age w)
  U.writeToLog $ "To " ++ agentId w
    ++ ", the vector has adjusted novelty " ++ show dObjNoveltyAdj
  mapM_ U.writeToLog $ scenarioReport sps
  mapM_ U.writeToLog $ responseReport rplos
  mapM_ U.writeToLog $ decisionReport aohs
  U.writeToLog $ agentId w ++ " chooses to " ++ show (view action r)
    ++ " predicting the outcomes " ++ show (view outcomes r)
  return (dObjNovelty, dObjNoveltyAdj, r, w')

analyseClassification
  :: [[(Cl.Label, Cl.Difference)]] -> PatternWain
    -> (Cl.Label, Cl.Difference, Int)
analyseClassification ldss w = (dObjLabel, dObjNovelty, dObjNoveltyAdj)
  where ((dObjLabel, dObjNovelty):_)
          = map (minimumBy (comparing snd)) ldss
        dObjNoveltyAdj
          = round $ uiToDouble dObjNovelty * fromIntegral (view W.age w)

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

describeModels
  :: PatternWain -> StateT (U.Universe PatternWain) IO ()
describeModels w = mapM_ (U.writeToLog . f) ms
  where ms = toList . modelMap . view (W.brain . predictor) $ w
        f (l, r) = view W.name w ++ "'s predictor model " ++ show l
                     ++ "=" ++ pretty r

describeOutcomes
  :: PatternWain -> [(Response Action, UIDouble, P.Label, [PM1Double])]
    -> StateT (U.Universe PatternWain) IO ()
describeOutcomes w = mapM_ (U.writeToLog . f)
  where f (r, _, l, _) = view W.name w ++ "'s predicted outcome of "
                     ++ show (view action r) ++ " is "
                     ++ concatMap (printf "%.3f" . pm1ToDouble) (view outcomes r)
                     ++ " from model " ++ show l

--
-- Utility functions
--

applyPopControl :: StateT Experiment IO ()
applyPopControl = do
  deltaE <- zoom (universe . U.uPopControlDeltaE) getPS
  adjustWainEnergy subject deltaE rPopControlDeltaE "pop control"

maybeFlirt :: StateT Experiment IO ()
maybeFlirt = do
  frequency <- use (universe . U.uFlirtingFrequency)
  x <- getRandom
  when (x < frequency) flirt

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  b <- use other
  babyName <- zoom universe U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ W.mate a b babyName
  if null msgs
    then do
      report $ agentId a ++ " and " ++ agentId b ++ " mated"
      report $ "Contribution to child: " ++
        agentId a ++ "'s share is " ++ show aMatingDeltaE ++ " " ++ 
        agentId b ++ "'s share is " ++ show bMatingDeltaE
      assign subject a'
      assign other b'
      recordBirths
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rOtherMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ report msgs

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (view W.litter a)

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- W.weanMatureChildren <$> use subject
  assign subject a
  (a':deadChildren) <- W.pruneDeadChildren <$> use subject
  assign subject a'
  assign weanlings (matureChildren ++ deadChildren)
  (summary.rWeanCount) += length matureChildren

killIfTooOld :: StateT Experiment IO ()
killIfTooOld = do
  a <- view W.age <$> use subject
  maxAge <- use (universe . U.uMaxAge)
  when (fromIntegral a > maxAge) $
    adjustWainEnergy subject (-100) rOldAgeDeltaE "old age"

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe PatternWain) IO ()
adjustPopControlDeltaE xs =
  unless (null xs) $ do
    let (Just average) = Stats.lookup "avg. energy" xs
    let (Just total) = Stats.lookup "total energy" xs
    budget <- use U.uEnergyBudget
    pop <- U.popSize
    let c = idealPopControlDeltaE average total budget pop
    U.writeToLog $ "Current avg. energy = " ++ show average
    U.writeToLog $ "Current total energy = " ++ show total
    U.writeToLog $ "energy budget = " ++ show budget
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

-- TODO: Make the 0.8 configurable
idealPopControlDeltaE :: Double -> Double -> Double -> Int -> Double
idealPopControlDeltaE average total budget pop
  | average < 0.8 = (budget - total) / (fromIntegral pop)
  | otherwise     = 0.8 - average

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view W.energy <$> use subject
  b <- fmap uiToDouble $ view W.energy <$> use other
  d <- W.childEnergy <$> use subject
  e <- W.childEnergy <$> use other
  return (a + b, d + e)

printStats
  :: [[Stats.Statistic]] -> StateT (U.Universe PatternWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

adjustWainEnergy
  :: Simple Lens Experiment PatternWain -> Double
    -> Simple Lens Summary Double -> String -> StateT Experiment IO ()
adjustWainEnergy
    wainSelector deltaE statLens reason = do
  w <- use wainSelector
  let (w', used) = W.adjustEnergy deltaE w
  report $ "Adjusting energy of " ++ agentId w
    ++ " because " ++ reason ++ ": " ++ show (view W.energy w)
    ++ " " ++ show deltaE
    ++ " -> " ++ show (view W.energy w')
    ++ " used=" ++ show used ++ " leftover=" ++ show (deltaE - used)
  (summary . statLens) += used
  assign wainSelector w'

letSubjectReflect
  :: PatternWain -> Response Action -> StateT Experiment IO ()
letSubjectReflect wBefore r = do
  w <- use subject
  p <- zoom (universe . U.uPrevVector) getPS
  let (w', err) = W.reflect [p] r wBefore w
  assign subject w'
  assign (summary . rRewardPredictionErr) err

writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe PatternWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"

reportAnyDeaths
  :: [PatternWain] -> StateT (U.Universe PatternWain) IO ()
reportAnyDeaths ws = mapM_ f ws
  where f w = when (not . isAlive $ w) $
                U.writeToLog
                  (agentId w ++ " dead at age "
                    ++ show (view W.age w))

-- mean :: (Eq a, Fractional a, Foldable t) => t a -> a
mean :: (Fractional a, Eq a) => [a] -> a
mean xs
  | count == 0 = error "no data"
  | otherwise = total / count
  where (total, count) = foldr f (0, 0) xs
        f x (y, n) = (y+x, n+1)
