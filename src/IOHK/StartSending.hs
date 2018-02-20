module IOHK.StartSending
    ( startSending
    ) where

import IOHK.Process

import Control.Concurrent (threadDelay)
import Control.Distributed.Process.Node

nOfProcesses :: Int
nOfProcesses = 4

microSecondsPerSecond :: Int
microSecondsPerSecond = 1000000

startSending :: Int -> Int -> IO ()
startSending duration seed = do
    (pids, node) <- createProcesses seed nOfProcesses
    threadDelay $ microSecondsPerSecond * duration
    runProcess node $ mapM_ endSending pids
    threadDelay 1000000
    closeLocalNode node
