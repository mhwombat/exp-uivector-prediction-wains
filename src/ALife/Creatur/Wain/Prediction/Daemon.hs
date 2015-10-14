------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Prediction.Daemon
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The daemon that runs the Créatúr experiment.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import ALife.Creatur.Daemon (CreaturDaemon(..), Job(..),
  simpleDaemon, launch)
import ALife.Creatur.Task (runInteractingAgents, simpleJob)
import ALife.Creatur.Wain.Prediction.Experiment (PredictorWain, run, 
  startRound, finishRound, versionInfo)
import ALife.Creatur.Wain.Prediction.Universe (Universe(..),
  writeToLog, loadUniverse, uSleepBetweenTasks, uExperimentName)
import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Control.Lens
import Control.Monad (unless)
import Control.Monad.State (execStateT)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Daemonize (CreateDaemon(name))

shutdownMessagePrinted :: MVar Bool
{-# NOINLINE shutdownMessagePrinted #-}
shutdownMessagePrinted = unsafePerformIO (newMVar False)

startupHandler :: String -> Universe PredictorWain -> IO (Universe PredictorWain)
startupHandler programName
  = execStateT (writeToLog $ "Starting " ++ programName)

shutdownHandler :: String -> Universe PredictorWain -> IO ()
shutdownHandler programName u = do
  -- Only print the message once
  handled <- readMVar shutdownMessagePrinted
  unless handled $ do
    _ <- execStateT (writeToLog $ "Shutdown requested for "
                      ++ programName) u
    _ <- swapMVar shutdownMessagePrinted True
    return ()

main :: IO ()
main = do
  u <- loadUniverse
  let program = run
  let message = versionInfo ++ ", configuration=" ++ show u
  let j = simpleJob
        { task=runInteractingAgents program startRound finishRound,
          onStartup=startupHandler message,
          onShutdown=shutdownHandler message,
          sleepTime=view uSleepBetweenTasks u }
  let d = (simpleDaemon j u) { name=Just . view uExperimentName $ u }
  let cd = CreaturDaemon d j
  launch cd
