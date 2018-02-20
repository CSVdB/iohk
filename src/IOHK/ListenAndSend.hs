{-# LANGUAGE FlexibleContexts #-}

module IOHK.ListenAndSend
    ( listenAndSend
    ) where

import Import

import IOHK.Info
import IOHK.Message
import IOHK.RNG

import Control.Distributed.Process

listenAndSend :: Info -> Process ()
listenAndSend info = do
    (newInfo, updated) <- listen info False
    if generatingMore newInfo
        then listenAndSend =<< generateAndSend newInfo
        else if updated
                 then listenAndSend newInfo
                 else finish newInfo

finish :: Info -> Process ()
finish info =
    liftIO . putStrLn $ concat [show $ nOfDoubles info, ", ", show $ total info]

listen :: Info -> Bool -> Process (Info, Bool)
listen info updated = do
    maybeMsg <- expectTimeout 0
    case maybeMsg of
        Nothing -> pure (info, updated)
        Just msg ->
            let newInfo = updateInfo msg info
            in listen newInfo True

generateAndSend :: Info -> Process Info
generateAndSend info = do
    let (xs, newGen) = generateDoubles (stdGen info) nOfDoublesPerMessage
    mapM_ (flip send $ RandomN xs) $ infoPids info
    pure info {stdGen = newGen}
