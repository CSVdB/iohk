{-# LANGUAGE RecordWildCards #-}

module IOHK
    ( iohk
    ) where

import IOHK.OptParse
import IOHK.StartSending

iohk :: IO ()
iohk = do
    instructions <- getInstructions
    execute instructions

execute :: Instructions -> IO ()
execute StartSending {..} = startSending duration grace seed
