module IOHK.Delay
    ( threadDelay
    , waitForXSeconds
    , microSecondsPerSecond
    ) where

import Control.Concurrent (threadDelay)

microSecondsPerSecond :: Int
microSecondsPerSecond = 1000000

waitForXSeconds :: Int -> IO ()
waitForXSeconds = threadDelay . (*) microSecondsPerSecond
