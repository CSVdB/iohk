module IOHK.RNG
    ( generateDoubles
    ) where

import System.Random

generateDoubles :: StdGen -> Int -> ([Double], StdGen)
generateDoubles stdGen 0 = ([], stdGen)
generateDoubles stdGen n =
    let (x, newGen) = randomR (0, 1) stdGen
        (xs, endStdGen) = generateDoubles newGen (n - 1)
    in (x : xs, endStdGen)
