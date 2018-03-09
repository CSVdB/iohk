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
updateInfo StopSending info = info {generatingMore = False}
updateInfo (PIDs pid) info = info {infoPids = pid}
updateInfo (RandomN xs) info =
    info
        { nOfDoubles = nOfDoublesPerMessage + nOfDoubles info
        , total = updateTotal (nOfDoubles info) (total info) xs
        }

updateTotal :: Int -> Double -> [Double] -> Double
updateTotal n oldTotal xs = (+) oldTotal . getNewTerm xs $ n + 1

getNewTerm :: [Double] -> Int -> Double
getNewTerm [] _ = 0
getNewTerm (x:xs) n = x * fromIntegral n + getNewTerm xs (n + 1)
