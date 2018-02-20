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

waitForXSeconds :: Int -> IO ()
waitForXSeconds = threadDelay . (*) microSecondsPerSecond

startSending :: Int -> Int -> Int -> IO ()
startSending duration grace seed = do
    (pids, node) <- createProcesses seed nOfProcesses
    waitForXSeconds duration
    runProcess node $ mapM_ endSending pids
    waitForXSeconds grace
    closeLocalNode node
