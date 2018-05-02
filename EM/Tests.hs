module Tests where

import Vector
import EM
import Data.List (transpose)
import Test.HUnit

v1 = [1.0,2,3]
v2 = [4.0,5,6]
m1 = [v1]
m2 = transpose [v2]
m3 = [[1.0,2,3], [4,5,6], [7,8,9]]
m3T = transpose m3

roundToDecimal places number = (fromInteger $ round $ number * (10^places)) / (10.0^^places)

matrixMultiplicationTest = TestList [
    (m1 *-* m2) ~?= [[32]],
    (m2 *-* m1) ~?= [[4, 8, 12], [5, 10, 15], [6, 12, 18]],
    (m1 *-* m3) ~?= [[30, 36, 42]],
    (m3 *-* m2) ~?= [[32], [77], [122]]
    ]

determinantTests = (det m3) ~?= 0

inverseTests =  [[0.5]] ~?= (inverse [[2.0]])

pseudoinverseTests = "Pseudo Inverse Tests" ~: (map (map (roundToDecimal 4)) $ pseudoinverse m3) ~?= [
    [-0.6389, -0.1667, 0.3056],
    [-0.0556, 0.0000, 0.0556],
    [0.5278, 0.1667, -0.1944]
    ] 

normalTest = TestList [
    0.2420 ~=?  (roundToDecimal 4 $ normal ([0], [[1]]) [1]),
    0.0540 ~=?  (roundToDecimal 4 $ normal ([0], [[1]]) [2]),
    0.01186 ~=? (roundToDecimal 5 $ normal ([0,0], [[2,0],[0,1]]) [1,2])
    ]
    
allTests = TestList [determinantTests, matrixMultiplicationTest, pseudoinverseTests, inverseTests, normalTest]

main = do
    putStrLn "Unit Tests:\n"
    runTestTT allTests