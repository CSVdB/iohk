module IOHK.StartSending
    ( startSending
    ) where

import IOHK.Delay
import IOHK.Process

import Control.Distributed.Process.Node

startSending :: Int -> Int -> Int -> IO ()
startSending duration grace seed = do
    (pids, node) <- createProcesses seed grace
    waitForXSeconds duration
    runProcess node $ mapM_ endSending pids
    waitForXSeconds grace
    closeLocalNode node
