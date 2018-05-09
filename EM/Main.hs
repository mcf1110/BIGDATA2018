module Main where

import System.IO
import System.Environment
import Formatting
import Formatting.Clock
import System.Clock

import Data.List.Split (splitOn)

import EM
import System.Random

parseLine :: String -> [Double]
parseLine line = map read $ take 3 $ tail $ splitOn "," line

main = do
    lns <- fmap (lines) $ readFile "./Data/songsdata.csv"
    points <- return $ map parseLine $ tail lns
    g <- getStdGen
    initialMM <- return $ initializeWithPoints
        $ map (\index -> points !! index) -- gets the actual points
        $ take 2 (randomRs (0, length points) g :: [Int]) -- takes 9 indices

    start <- getTime Monotonic
    let finalMM = iterateEM points initialMM 1
    let ll = logLikelihood points
    putStrLn "Melhora no likelihood depois de 1 iteracao :"
    print $ ll finalMM - ll initialMM
    stop <- getTime Monotonic
    fprint (timeSpecs) start stop
