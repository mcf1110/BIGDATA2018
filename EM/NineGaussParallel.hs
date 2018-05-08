module Main where

import System.IO
import System.Environment
import Formatting
import Formatting.Clock
import System.Clock

import EMParmap
import System.Random
import Data.List.Split (chunksOf)

parseLine :: String -> [Double]
parseLine line = map read $ words line

main = do
    lns <- fmap (drop 1 . lines) $ readFile "./Data/9Gauss.txt"
    points <- return $ map parseLine lns
    chunks <- return $ chunksOf 100 points
    g <- getStdGen
    initialMM <- return $ initializeWithPoints
        $ map (\index -> points !! index) -- gets the actual points
        $ take 9 (randomRs (0, length points) g :: [Int]) -- takes 9 indices

    start <- getTime Monotonic
    let finalMM = iterateEM chunks initialMM 100
    let ll = logLikelihood chunks
    putStrLn "Melhora no likelihood depois de 100 iteracoes :"
    print $ ll finalMM - ll initialMM
    stop <- getTime Monotonic
    fprint (timeSpecs) start stop
