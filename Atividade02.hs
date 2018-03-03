-- Exercícios sobre Funções
module Atividade02 where
import Data.Char (digitToInt)
{-
Exercício 01: Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.
-}
ehTriangulo :: Integer -> Integer -> Integer -> Bool
ehTriangulo x y z
    | x >= y + z = False
    | y >= x + z = False
    | z >= x + y = False
    | otherwise = True

exercicio01 = ehTriangulo

{-
Exercício 02: Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.
-}

data TipoTriangulo = Equilatero | Isosceles | Escaleno | NaoEhTriangulo deriving Show

tipoTriangulo :: Integer -> Integer -> Integer -> TipoTriangulo
tipoTriangulo x y z
    | x == y && y == z = Equilatero
    | x == y || y == z || z == x = Isosceles
    | ehTriangulo x y z = Escaleno
    | otherwise = NaoEhTriangulo

exercicio02 = tipoTriangulo

{-
Exercício 03: Implemente uma função que faz a multiplicação etíope entre dois números.
-}

etiope :: Integer -> Integer -> Integer
etiope 1 y = y
etiope x y
    | even x = etiope (div x 2) (2*y)
    | otherwise = y + etiope (div x 2) (2*y)

exercicio03 = etiope

{-
Exercício 04: Faça uma função que determine se um número é primo.
-}
ehPrimo :: Integer -> Bool
ehPrimo n = null $ filter (isDivisibleBy n) [2..(n-1)]
    where isDivisibleBy x y = rem x y == 0

exercicio04 = ehPrimo

{-
Exercício 05: Faça uma função que calcule a soma dos dígitos de um número.
-}
somaDigitos :: Integer -> Integer
somaDigitos =  toInteger . sum . map digitToInt . show

exercicio05 = somaDigitos

{-
Exercício 06: Faça uma função que calcule a persistência aditiva de um número.
-}
persistenciaAditiva :: Integer -> Integer
persistenciaAditiva x = pa' x 0
    where 
    pa' x cont 
        | x < 10 = cont 
        | otherwise = pa' (somaDigitos x) (cont + 1)

exercicio06 = persistenciaAditiva

{-
Exercício 07: Faça uma função que calcule o coeficiente binomial de (m,n).
-}
coefBinomial :: Integer -> Integer -> Integer
coefBinomial m n = div (product [(m-n+1)..m]) (product [1..n])

exercicio07 = coefBinomial

{-
Exercício 08: Faça uma função que calcule o elemento (i,j) do triângulo de pascal.
-}
pascal = coefBinomial
exercicio08 = exercicio07