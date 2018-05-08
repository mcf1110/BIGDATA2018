module Main where

import System.IO
import System.Environment
import Formatting
import Formatting.Clock
import System.Clock

import EM
import System.Random

parseLine :: String -> [Double]
parseLine line = map read $ words line

main = do
    lns <- fmap (drop 1 . lines) $ readFile "./Data/9Gauss.txt"
    points <- return $ map parseLine lns
    g <- getStdGen
    initialMM <- return $ initializeWithPoints
        $ map (\index -> points !! index) -- gets the actual points
        $ take 9 (randomRs (0, length points) g :: [Int]) -- takes 9 indices

    start <- getTime Monotonic
    let finalMM = iterateEM points initialMM 100
    let ll = logLikelihood points
    putStrLn "Melhora no likelihood depois de 50 iteracoes :"
    print $ ll finalMM - ll initialMM
    stop <- getTime Monotonic
    fprint (timeSpecs) start stop
