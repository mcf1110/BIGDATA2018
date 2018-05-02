{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  NoMonomorphismRestriction #-}
{-|
Module      : Vectors
Description : operações com vetores
Copyright   : (c) Fabrício Olivetti, 2018
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Extended by Matheus Fernandes (mcf1110)

Exemplifies the use of Maybe type
-}

module Vector where

import Data.List (transpose, foldl1', intercalate)

type Vector a = [a]

type Matrix a = [[a]]

-- scalar - vector operators
(.+) xs x = map (+x) xs
(.-) xs x = map ((-)x) xs
(.*) xs x = map (*x) xs
(./) xs x = map (/x) xs
(.^) xs x = map (^x) xs
(.**) xs x = map (**x) xs

(+.) x xs = map (+x) xs
(*.) x xs = map (*x) xs

-- vector - vector operators
(.+.) = zipWith (+)
(.-.) = zipWith (-)
(.*.) = zipWith (*)
(./.) = zipWith (/)

-- scalar - matrix operators
(..+) xs x = map (.+x) xs
(..-) xs x = map ((.-)x) xs
(..*) xs x = map (.*x) xs
(../) xs x = map (./x) xs
(..^) xs x = map (.^x) xs
(..**) xs x = map (.**x) xs

(+..) x xs = map (x+.) xs
(*..) x xs = map (x*.) xs

-- vector - matrix operators
(..+.) xs x = map (.+.x) xs
(..-.) xs x = map (.-.x) xs
(..*.) xs x = map (.*.x) xs
(.*..) x xs = map (.*.x) xs
(../.) xs x = map (./.x) xs

-- matrix - matrix operators
(..+..) = zipWith (.+.)
(..-..) = zipWith (.-.)
(..*..) = zipWith (.*.)
(../..) = zipWith (./.)

dotprod :: Num a => Vector a -> Vector a -> a
dotprod u v = sum $ u .*. v

norm u = dotprod u u

mediaVetor :: Fractional a => Matrix a -> Vector a
mediaVetor xs = sumVec ./ n
  where
    sumVec = foldl1' (.+.) xs
    n      = fromIntegral $ length xs

mapColunas :: (Vector a -> Vector a) -> Matrix a -> Matrix a
mapColunas f m = map f $ transpose m

outerprod :: Num a => Vector a -> Vector a -> Matrix a
outerprod u v = [ [ui * vi | vi <- v] | ui <- u ]

reshape :: Int -> Vector a -> Matrix a
reshape n [] = []
reshape n xs = (take n xs) : (reshape n (drop n xs))

length' xs = fromIntegral $ length xs

epsilon = 1e-6

-- DETERMINANT
-- From: https://github.com/frostblooded/haskell-determinant

-- Remove col i from the passed row
removeCol :: Num a => Int -> Vector a -> Vector a
removeCol 0 (_:rs) = rs
removeCol i (r:rs) = r : removeCol (i - 1) rs

-- Remove col i from all rows of the matrix
removeCols :: Num a => Int -> Matrix a -> Matrix a
removeCols i = map $ removeCol i

-- Calculate one element from the sum of the matrix
getDetSumElement :: Num a => Matrix a -> Int -> a
getDetSumElement matrix i = sign * element * detSum restOfTheMatrix 0
    where
        sign = (-1) ^ i
        element = matrix !! 0 !! i
        matrixWithoutFirstRow = tail matrix
        restOfTheMatrix = removeCols i matrixWithoutFirstRow


detSum :: Num a => Matrix a -> Int -> a
detSum matrix i 
    | len == 1 = matrix !! 0 !! 0
    | len == i = 0
    | otherwise = currentSumElement + detSum matrix (i + 1)
        where
           currentSumElement = getDetSumElement matrix i
           len = length matrix

isSquare :: Num a => Matrix a -> Int -> Bool
isSquare [] _ = True
isSquare (r:rows) len = len == length r && isSquare rows len


isValid :: Num a => Matrix a -> Bool
isValid matrix = isSquare matrix (length matrix)

det :: Num a => Matrix a -> a
det matrix 
    | notValid matrix = error "Matriz não quadrada!"
    | otherwise = detSum matrix 0
        where
            notValid = not . isValid
pseudodet :: (Fractional a, Eq a) => Matrix a -> a
pseudodet m
    | determinant == 0 = det (addToMainDiagonal epsilon m)
    | otherwise = determinant
    where determinant = det m

-- COFACTOR
removeRow :: Num a => Int -> Matrix a -> Matrix a
removeRow 0 (_:rest) = rest
removeRow n (r:rest) = r : removeRow (n-1) rest


cofactor :: Num a => Int -> Int -> Matrix a -> a
cofactor i j matrix = (-1)^(i+j) * minor i j matrix
    where 
        minor i j matrix = det $ removeRow i $ removeCols j matrix

cofactorMatrix :: Num a => Matrix a -> Matrix a
cofactorMatrix m = reshape size [cofactor i j m | i <- [0..(size-1)], j <- [0..(size-1)]]
    where size = length m


-- INVERSE
shape :: Matrix a -> (Int, Int)
shape m = (length m, length $ head m)


adjugateMatrix :: Num a => Matrix a -> Matrix a
adjugateMatrix = transpose . cofactorMatrix

inverse :: (Eq a, Fractional a) => Matrix a -> Matrix a
inverse m 
    | determinant == 0 = inverse (addToMainDiagonal epsilon m)
    | length m == 1 = [[1 / (head $ head m)]]
    | otherwise = adjugateMatrix m ../ determinant
        where determinant = det m


pseudoinverse :: (Eq a, Fractional a) => Matrix a -> Matrix a
pseudoinverse m = (inverse (mT *-* m)) *-* mT where mT = transpose m

--IDENTIDADE
identity :: Num a => Int -> Matrix a
identity n = makeId 0 n
    where 
        zeros n = take n $ repeat 0
        makeId index n
            | index >= n = []
            | otherwise = (zeros index ++ [1] ++ zeros (n - index - 1)) : makeId (index +1 ) n

addToMainDiagonal :: Num a => a -> Matrix a -> Matrix a
addToMainDiagonal x matrix = [ addToLineAtPosition x p $ matrix !! p | p <- [0..size]]
    where
        size = (length matrix) - 1
        addToLineAtPosition :: Num a => a -> Int -> [a] -> [a]
        addToLineAtPosition value pos line = (take pos line) ++ [(value + x)] ++ rest where (x:rest) = drop pos line

-- MULTIPLICACAO DE MATRIZES
{-(*-*) :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
(*-*) m n
    | (snd $ shape m) == (fst $ shape n) = Just (matrixmult m n)
    | otherwise = Nothing
    where 
        matrixmult m n = reshape (length m) $ [ sum $ x .*. y | x <- m, y <- nt ] where nt = transpose n-}
(*-*) m n = reshape (length $ head $ n) $ [ sum $ x .*. y | x <- m, y <- nt ] where nt = transpose n

showM matrix = putStrLn $ intercalate "\n" $ map show matrix