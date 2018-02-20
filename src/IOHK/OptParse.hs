module IOHK.OptParse
    ( getInstructions
    , Instructions(..)
    ) where

import Import

import Text.Read

import System.Environment

errMess :: String
errMess =
    "Input should be of the form \"startServer x y\" with x how long nodes should send messages (in s), y the grace period (in s) and z the seed."

getInstructions :: IO Instructions
getInstructions = do
    args <- getArgs
    case args of
        ["startServer", x, y, z] ->
            case StartSending <$> readMaybe x <*> readMaybe y <*> readMaybe z of
                Just instr -> pure instr
                Nothing -> die errMess
        _ -> die errMess

data Instructions = StartSending
    { duration :: Int
    , grace :: Int
    , seed :: Int
    } deriving (Show, Eq)
