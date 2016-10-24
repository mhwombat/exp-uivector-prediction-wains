------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UIVector.Prediction.DataSource
-- Copyright   :  (c) Amy de Buitléir 2015-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Reader of data files.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.UIVector.Prediction.DataSource where

import ALife.Creatur.Persistent (Persistent, mkPersistent, getPS, putPS)
import ALife.Creatur.Util (modifyLift, stateMap)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (StateT, get, gets)
import Data.List.Split (splitOn)
import System.IO (Handle, hGetLine, hIsEOF, IOMode(ReadMode), openFile)
import Text.Read (readEither)

data DataSource = DataSource
  {
    initialised :: Bool,
    currentTime :: Persistent Int,
    fileHandle :: Handle,
    fileName :: FilePath
  }

instance Show DataSource where
  show d = "DataSource {initialised = " ++ show (initialised d)
           ++ ", currentTime = " ++ show (currentTime d)
           ++ ", fileHandle = <not shown>"
           ++ ", fileName = " ++ show (fileName d)

mkDataSource :: FilePath -> FilePath -> DataSource
mkDataSource dataFile counterFile = DataSource False counter undefined dataFile
  where counter = mkPersistent (-1) counterFile

initIfNeeded :: StateT DataSource IO ()
initIfNeeded = do
  isInitialised <- gets initialised
  unless isInitialised $ modifyLift initialise

initialise :: DataSource -> IO DataSource
initialise d = do
  h <- openFile (fileName d) ReadMode
  return d { initialised = True, fileHandle = h }

-- TODO: Close file properly when end of data reached.
-- TODO: At the moment the file will stay open until the daemon shuts down.
-- TODO: Also delete the currentTime file for the next run?

endOfData :: StateT DataSource IO Bool
endOfData = do
  initIfNeeded
  h <- gets fileHandle
  liftIO $ hIsEOF h

nextVector :: StateT DataSource IO [UIDouble]
nextVector = do
  initIfNeeded
  d <- get
  s <- liftIO $ hGetLine (fileHandle d)
  let (t, xs) = parseLine "," s
  tPrev <- stateMap (\tPs -> d { currentTime = tPs }) currentTime getPS
  if t > tPrev
    then do
      stateMap (\tPs -> d { currentTime = tPs }) currentTime (putPS t)
      return xs
    else nextVector

parseLine :: String -> String -> (Int, [UIDouble])
parseLine delimiter text = (t, xs)
  where (s:ss) = splitOn delimiter text
        t = safeRead s
        xs = map safeRead ss

safeRead :: Read a => String -> a
safeRead s = case readEither s of
               Left msg -> error $
                            "Unable to parse '" ++ s ++ "': " ++ msg
               Right x  -> x
