module IOHK.Process
    ( createProcesses
    , endSending
    ) where

import IOHK.Info
import IOHK.ListenAndSend
import IOHK.Message

import Control.Distributed.Process
import Control.Distributed.Process.Node

import Network.Transport.TCP
       (createTransport, defaultTCPParameters)

import System.Random

createProcesses :: Int -> Int -> IO ([ProcessId], LocalNode)
createProcesses seed n = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    pids <- createNewProcesses seed n node []
    pure (pids, node)

createNewProcesses :: Int -> Int -> LocalNode -> [ProcessId] -> IO [ProcessId]
createNewProcesses _ 0 _ pids = pure pids
createNewProcesses seed n node pids = do
    newPid <- forkProcess node $ startProcess seed pids
    createNewProcesses seed (n - 1) node $ newPid : pids

startProcess :: Int -> [ProcessId] -> Process ()
startProcess seed pids = do
    pid <- getSelfPid
    mapM_ (flip send $ PID pid) pids
    listenAndSend $ Info 0 0.0 (pid : pids) True $ mkStdGen seed

endSending :: ProcessId -> Process ()
endSending pid = send pid StopSending
