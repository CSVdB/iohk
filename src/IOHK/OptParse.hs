module IOHK.OptParse
    ( getInstructions
    , Instructions(..)
    ) where

import Import

import Text.Read

import System.Environment

errMess :: String
errMess =
    "Input should be of the form \"startServer x y\" with x the number of seconds the nodes should send and y the seed."

getInstructions :: IO Instructions
getInstructions = do
    args <- getArgs
    case args of
        ["startServer", x, y] ->
            case StartSending <$> readMaybe x <*> readMaybe y of
                Just instr -> pure instr
                Nothing -> die errMess
        _ -> die errMess

data Instructions = StartSending
    { duration :: Int
    , seed :: Int
    } deriving (Show, Eq)
