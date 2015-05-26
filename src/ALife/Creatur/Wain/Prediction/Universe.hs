------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.Universe
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Universe for image mining agents
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Prediction.Universe
  (
    -- * Constructors
    Universe(..),
    loadUniverse,
    U.Agent,
    -- * Lenses
    uExperimentName,
    uClock,
    uLogger,
    uDB,
    uNamer,
    uChecklist,
    uStatsFile,
    uRawStatsFile,
    uShowDeciderModels,
    uShowPredictions,
    uSleepBetweenTasks,
    uVectorLength,
    uClassifierSizeRange,
    uDeciderSizeRange,
    uDevotionRange,
    uMaturityRange,
    uMaxAge,
    uInitialPopulationSize,
    uIdealPopulationSize,
    uPopulationAllowedRange,
    uNumPredictionsToReward,
    uTopPredictionsDeltaE,
    uBaseMetabolismDeltaE,
    uEnergyCostPerByte,
    uChildCostFactor,
    uFlirtingFrequency,
    uPopControlDeltaE,
    uClassifierR0Range,
    uClassifierDRange,
    uDeciderR0Range,
    uDeciderDRange,
    uCheckpoints,
    uCurrVector,
    uPrevVector,
    uPredictions,
    uRewards,
    -- * Other
    U.agentIds,
    U.currentTime,
    U.genName,
    U.getAgent,
    U.popSize,
    U.store,
    U.writeToLog
  ) where

import qualified ALife.Creatur as A
import qualified ALife.Creatur.Namer as N
import qualified ALife.Creatur.Checklist as CL
import qualified ALife.Creatur.Counter as K
import qualified ALife.Creatur.Database as D
import qualified ALife.Creatur.Database.CachedFileSystem as CFS
import qualified ALife.Creatur.Logger.SimpleLogger as SL
import ALife.Creatur (AgentId)
import ALife.Creatur.Persistent (Persistent, mkPersistent)
import qualified ALife.Creatur.Universe as U
import qualified ALife.Creatur.Wain.Checkpoint as CP
import ALife.Creatur.Wain.Prediction.Action (Action)
import ALife.Creatur.Wain.Response (Response)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Exception (SomeException, try)
import Control.Lens hiding (Setting)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings)
import Data.Word (Word16)
import System.Directory (makeRelativeToCurrentDirectory)

data Universe a = Universe
  {
    _uExperimentName :: String,
    _uClock :: K.PersistentCounter,
    _uLogger :: SL.SimpleLogger,
    _uDB :: CFS.CachedFSDatabase a,
    _uNamer :: N.SimpleNamer,
    _uChecklist :: CL.PersistentChecklist,
    _uStatsFile :: FilePath,
    _uRawStatsFile :: FilePath,
    _uShowDeciderModels :: Bool,
    _uShowPredictions :: Bool,
    _uSleepBetweenTasks :: Int,
    _uVectorLength :: Int,
    _uClassifierSizeRange :: (Word16, Word16),
    _uDeciderSizeRange :: (Word16, Word16),
    _uDevotionRange :: (Double, Double),
    _uMaturityRange :: (Word16, Word16),
    _uMaxAge :: Int,
    _uInitialPopulationSize :: Int,
    _uIdealPopulationSize :: Int,
    _uPopulationAllowedRange :: (Int, Int),
    _uNumPredictionsToReward :: Int,
    _uTopPredictionsDeltaE :: Double,
    _uBaseMetabolismDeltaE :: Double,
    _uEnergyCostPerByte :: Double,
    _uChildCostFactor :: Double,
    _uFlirtingFrequency :: Double,
    _uPopControlDeltaE :: Persistent Double,
    _uClassifierR0Range :: (Double,Double),
    _uClassifierDRange :: (Double,Double),
    _uDeciderR0Range :: (Double,Double),
    _uDeciderDRange :: (Double,Double),
    _uCheckpoints :: [CP.Checkpoint],
    _uCurrVector :: Persistent [UIDouble],
    _uPrevVector :: Persistent [UIDouble],
    _uPredictions :: Persistent [(AgentId, (Response Action, UIDouble))],
    _uRewards :: Persistent [(AgentId, (Response Action, Double))]
  } deriving Show
makeLenses ''Universe

instance (A.Agent a, D.SizedRecord a) => U.Universe (Universe a) where
  type Agent (Universe a) = a
  type Clock (Universe a) = K.PersistentCounter
  clock = _uClock
  setClock u c = u { _uClock=c }
  type Logger (Universe a) = SL.SimpleLogger
  logger = _uLogger
  setLogger u l = u { _uLogger=l }
  type AgentDB (Universe a) = CFS.CachedFSDatabase a
  agentDB = _uDB
  setAgentDB u d = u { _uDB=d }
  type Namer (Universe a) = N.SimpleNamer
  agentNamer = _uNamer
  setNamer u n = u { _uNamer=n }
  type Checklist (Universe a) = CL.PersistentChecklist
  checklist = _uChecklist
  setChecklist u cl = u { _uChecklist=cl }

requiredSetting :: String -> Setting a
requiredSetting key
  = Setting key (error $ key ++ " not defined in configuration")

cExperimentName :: Setting String
cExperimentName = requiredSetting "experimentName"

cWorkingDir :: Setting FilePath
cWorkingDir = requiredSetting "workingDir"

cCacheSize :: Setting Int
cCacheSize = requiredSetting "cacheSize"

cShowDeciderModels :: Setting Bool
cShowDeciderModels = requiredSetting "showDeciderModels"

cShowPredictions :: Setting Bool
cShowPredictions = requiredSetting "showPredictions"

cSleepBetweenTasks :: Setting Int
cSleepBetweenTasks = requiredSetting "sleepTimeBetweenTasks"

cVectorLength :: Setting Int
cVectorLength = requiredSetting "vectorLength"

cClassifierSizeRange :: Setting (Word16, Word16)
cClassifierSizeRange
  = requiredSetting "classifierSizeRange"

cDeciderSizeRange :: Setting (Word16, Word16)
cDeciderSizeRange
  = requiredSetting "deciderSizeRange"
    
cDevotionRange :: Setting (Double, Double)
cDevotionRange
  = requiredSetting "devotionRange"

cMaturityRange :: Setting (Word16, Word16)
cMaturityRange = requiredSetting "maturityRange"

cMaxAge :: Setting Int
cMaxAge = requiredSetting "maxAge"

cInitialPopulationSize :: Setting Int
cInitialPopulationSize = requiredSetting "initialPopSize"

cIdealPopulationSize :: Setting Double
cIdealPopulationSize = requiredSetting "idealPopSize"

cPopulationAllowedRange :: Setting (Double, Double)
cPopulationAllowedRange = requiredSetting "popAllowedRange"

cNumPredictionsToReward :: Setting Int
cNumPredictionsToReward = requiredSetting "numPredictionsToReward"

cTopPredictionsDeltaE :: Setting Double
cTopPredictionsDeltaE = requiredSetting "topPredictionsDeltaE"

cBaseMetabolismDeltaE :: Setting Double
cBaseMetabolismDeltaE = requiredSetting "baseMetabDeltaE"

cEnergyCostPerByte :: Setting Double
cEnergyCostPerByte = requiredSetting "energyCostPerByte"

cChildCostFactor :: Setting Double
cChildCostFactor = requiredSetting "childCostFactor"

cFlirtingFrequency :: Setting Double
cFlirtingFrequency = requiredSetting "flirtingFrequency"

cClassifierR0Range :: Setting (Double,Double)
cClassifierR0Range = requiredSetting "classifierR0Range"

cClassifierDRange :: Setting (Double,Double)
cClassifierDRange = requiredSetting "classifierDecayRange"

cDeciderR0Range :: Setting (Double,Double)
cDeciderR0Range = requiredSetting "deciderR0Range"

cDeciderDRange :: Setting (Double,Double)
cDeciderDRange = requiredSetting "deciderDecayRange"

cCheckpoints :: Setting [CP.Checkpoint]
cCheckpoints = requiredSetting "checkpoints"

loadUniverse :: IO (Universe a)
loadUniverse = do
  configFile <- Path <$> makeRelativeToCurrentDirectory "prediction.config"
  readResult <- try $ readSettings configFile
  case readResult of
    Right (_, GetSetting getSetting) -> return $
            config2Universe getSetting
    Left (x :: SomeException) -> error $
            "Error reading the config file: " ++ show x

config2Universe :: (forall a. Read a => Setting a -> a) -> Universe b
config2Universe getSetting =
  Universe
    {
      _uExperimentName = en,
      _uClock = K.mkPersistentCounter (workDir ++ "/clock"),
      _uLogger = SL.mkSimpleLogger (workDir ++ "/log/" ++ en ++ ".log"),
      _uDB
        = CFS.mkCachedFSDatabase (workDir ++ "/db")
          (getSetting cCacheSize),
      _uNamer = N.mkSimpleNamer (en ++ "_") (workDir ++ "/namer"),
      _uChecklist = CL.mkPersistentChecklist (workDir ++ "/todo"),
      _uStatsFile = workDir ++ "/statsFile",
      _uRawStatsFile = workDir ++ "/rawStatsFile",
      _uShowDeciderModels = getSetting cShowDeciderModels,
      _uShowPredictions = getSetting cShowPredictions,
      _uSleepBetweenTasks = getSetting cSleepBetweenTasks,
      _uVectorLength = getSetting cVectorLength,
      _uClassifierSizeRange = getSetting cClassifierSizeRange,
      _uDeciderSizeRange = getSetting cDeciderSizeRange,
      _uDevotionRange = getSetting cDevotionRange,
      _uMaturityRange = getSetting cMaturityRange,
      _uMaxAge = getSetting cMaxAge,
      _uInitialPopulationSize = p0,
      _uIdealPopulationSize = pIdeal,
      _uPopulationAllowedRange = (a', b'),
      _uNumPredictionsToReward = getSetting cNumPredictionsToReward,
      _uTopPredictionsDeltaE = getSetting cTopPredictionsDeltaE,
      _uBaseMetabolismDeltaE = getSetting cBaseMetabolismDeltaE,
      _uEnergyCostPerByte = getSetting cEnergyCostPerByte,
      _uChildCostFactor = getSetting cChildCostFactor,
      _uFlirtingFrequency = getSetting cFlirtingFrequency,
      _uPopControlDeltaE
        = mkPersistent 0 (workDir ++ "/popControlDeltaE"),
      _uClassifierR0Range = getSetting cClassifierR0Range,
      _uClassifierDRange = getSetting cClassifierDRange,
      _uDeciderR0Range = getSetting cDeciderR0Range,
      _uDeciderDRange = getSetting cDeciderDRange,
      _uCheckpoints = getSetting cCheckpoints,
      _uCurrVector = mkPersistent [] (workDir ++ "/currVector"),
      _uPrevVector = mkPersistent [] (workDir ++ "/prevVector"),
      _uPredictions = mkPersistent [] (workDir ++ "/predictions"),
      _uRewards = mkPersistent [] (workDir ++ "/rewards")
    }
  where en = getSetting cExperimentName
        workDir = getSetting cWorkingDir
        p0 = getSetting cInitialPopulationSize
        fIdeal = getSetting cIdealPopulationSize
        pIdeal = round (fromIntegral p0 * fIdeal)
        (a, b) = getSetting cPopulationAllowedRange
        a' = round (fromIntegral pIdeal * a)
        b' = round (fromIntegral pIdeal * b)
