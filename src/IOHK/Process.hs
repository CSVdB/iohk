module IOHK.Process
    ( createProcesses
    , endSending
    , nOfProcesses
    ) where

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
createProcesses seed grace = createNProcesses seed grace nOfProcesses

createNProcesses :: Int -> Int -> Int -> IO ([ProcessId], LocalNode)
createNProcesses seed grace n = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    pids <- createNewProcesses seed grace n node []
    pure (pids, node)

createNewProcesses ::
       Int -> Int -> Int -> LocalNode -> [ProcessId] -> IO [ProcessId]
createNewProcesses _ _ 0 _ pids = pure pids
createNewProcesses seed grace n node pids = do
    newPid <- forkProcess node $ startProcess seed grace pids
    createNewProcesses seed grace (n - 1) node $ newPid : pids

startProcess :: Int -> Int -> [ProcessId] -> Process ()
startProcess seed grace pids = do
    pid <- getSelfPid
    mapM_ (flip send $ PID pid) pids
    startListening nOfProcesses grace $
        Info 0 0.0 (pid : pids) True $ mkStdGen seed

endSending :: ProcessId -> Process ()
endSending pid = send pid StopSending
