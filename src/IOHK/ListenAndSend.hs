{-# LANGUAGE FlexibleContexts #-}

module IOHK.ListenAndSend
    ( startListening
    ) where

import Import

import IOHK.Delay
import IOHK.Info
import IOHK.Message
import IOHK.RNG

import Control.Distributed.Process

startListening :: Int -> Int -> Info -> Process ()
startListening n grace info = do
    (newInfo, _) <- listen False info
    if length (infoPids newInfo) == n
        then listenAndSend grace newInfo
        else startListening n grace newInfo

listenAndSend :: Int -> Info -> Process ()
listenAndSend grace info = do
    (newInfo, updated) <- listen False info
    if generatingMore newInfo
        then listenAndSend grace =<< generateAndSend newInfo
        else if updated
                 then listenAndSend grace newInfo
                 else finish grace newInfo

finish :: Int -> Info -> Process ()
finish grace info = do
    liftIO . threadDelay $ grace * microSecondsPerSecond
    (newInfo, _) <- listen False info
    liftIO . putStrLn $
        concat [show $ nOfDoubles newInfo, ", ", show $ total newInfo]

listen :: Bool -> Info -> Process (Info, Bool)
listen updated info = do
    maybeMsg <- expectTimeout 0
    case maybeMsg of
        Nothing -> pure (info, updated)
        Just msg -> listen True $ updateInfo msg info

generateAndSend :: Info -> Process Info
generateAndSend info = do
    let (xs, newGen) = generateDoubles (stdGen info) nOfDoublesPerMessage
    mapM_ (flip send $ RandomN xs) $ infoPids info
    pure info {stdGen = newGen}
