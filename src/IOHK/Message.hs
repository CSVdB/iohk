{-# LANGUAGE DeriveGeneric #-}

module IOHK.Message
    ( MyMessage(..)
    , nOfDoublesPerMessage
    ) where

import Import

import Control.Distributed.Process

data MyMessage
    = StopSending
    | PID ProcessId
    | RandomN [Double]
    deriving (Show, Eq, Generic)

instance Binary MyMessage

nOfDoublesPerMessage :: Int
nOfDoublesPerMessage = 20000
