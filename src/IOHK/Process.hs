module IOHK.Process
    ( createProcesses
    , endSending
    , nOfProcesses
    ) where

import Import

import IOHK.Info
import IOHK.ListenAndSend
import IOHK.Message

import Control.Distributed.Process
import Control.Distributed.Process.Node

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import System.Random

nOfProcesses :: Int
nOfProcesses = 4

createProcesses :: Int -> Int -> IO ([ProcessId], LocalNode)
createProcesses seed grace = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    pids <- createNewProcesses seed grace nOfProcesses node
    pure (pids, node)

createNewProcesses :: Int -> Int -> Int -> LocalNode -> IO [ProcessId]
createNewProcesses seed grace n node = do
    pids <- replicateM n $ forkProcess node $ startProcess seed grace
    _ <- forkProcess node $ sendPids pids
    pure pids

sendPids :: [ProcessId] -> Process ()
sendPids pids = forM_ pids $ flip send $ PIDs pids

startProcess :: Int -> Int -> Process ()
startProcess seed grace =
    startListening nOfProcesses grace $ Info 0 0.0 [] True $ mkStdGen seed

endSending :: ProcessId -> Process ()
endSending pid = send pid StopSending
