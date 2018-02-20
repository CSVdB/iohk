{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module IOHK.Info
    ( Info(..)
    , updateInfo
    ) where

import IOHK.Message

import Control.Distributed.Process

import System.Random

data Info = Info
    { nOfDoubles :: Int
    , total :: Double
    , infoPids :: [ProcessId]
    , generatingMore :: Bool
    , stdGen :: StdGen
    } deriving (Show)

updateInfo :: MyMessage -> Info -> Info
updateInfo StopSending Info {..} = Info nOfDoubles total infoPids False stdGen
updateInfo (PID pid) Info {..} =
    Info nOfDoubles total (pid : infoPids) generatingMore stdGen
updateInfo (RandomN xs) Info {..} =
    Info
        (nOfDoubles + nOfDoublesPerMessage)
        (updateTotal nOfDoubles total xs)
        infoPids
        generatingMore
        stdGen

updateTotal :: Int -> Double -> [Double] -> Double
updateTotal nOfDoubles total xs = (+) total . getNewTerm xs $ nOfDoubles + 1

getNewTerm :: [Double] -> Int -> Double
getNewTerm [] _ = 0
getNewTerm (x:xs) n = x * fromIntegral n + getNewTerm xs (n + 1)
