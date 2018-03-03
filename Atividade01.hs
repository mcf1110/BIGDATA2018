--Exercícios Básicos
module Atividade01 where
import Data.Char (digitToInt)

{-
Exercício 01: Execute as seguintes operações utilizando o menor número de parênteses:

2⋅3+5
2+2⋅3+1
3^4+5⋅2^5+1
-}

exercicio01 :: (Int, Int, Int)
exercicio01 = (2*3+5 , 2+2*3+1 , 3^4+5*2^5+1) --os parenteses aqui são só para retornar uma tupla arrumadinha

{-
Exercício 02: Faça uma função mult3 x que retorne True caso a entrada seja múltiplo de 3 e False caso contrário.
-}
mult3 :: Int -> Bool
mult3 x = rem x 3 == 0
-- mult3 = (==) 0 . flip rem 3 --versão pointfree

mult :: Int -> Int -> Bool
mult y x = rem x y == 0 -- abstracao
mult3' = mult 3 --usando a abstracao

{-
Exercício 03: Faça uma função mult5 x que retorne True caso a entrada seja múltiplo de 5 e False caso contrário.
-}
mult5 :: Int -> Bool
mult5 = mult 5 --usando a abstracao, mas podia ser feito exatamente igual ao anterior, só substituindo

{-
Exercício 04: Faça uma função mult35 x que retorne True caso a entrada seja múltiplo de 3 e 5 e False caso contrário.
-}
mult35 :: Int -> Bool
mult35 x = mult5 x && mult3 x

exercicio02 = mult3
exercicio03 = mult5
exercicio04 = mult35

{-
Exercício 05: Faça um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E múltiplo de 2), e False caso contrário.
-}
exercicio05 :: Int -> Bool
exercicio05 x = x < -1 || x > 1 && mult 2 x

{-
Exercício 06: Faça uma função que recebe um tipo Integer e retorna ele dividido por 2:
-}
exercicio06 :: Integer -> Double
exercicio06 x = (fromInteger x) / 2

{-
Exercício 07: Faça uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo utilizando a identidade:
-}
exercicio07 :: Float -> (Float, Float)
exercicio07 ang = (identidade, -identidade)
    where identidade = sqrt ((1 - (cos ang))/2)

{-
Exercício 08: Crie uma lista de anos bissextos desde o ano 1 até o atual.
-}
isBissexto a = mult 4 a && not (mult 100 a) || mult 400 a
exercicio08 :: [Int]
exercicio08 = filter isBissexto [1..2018]

{-
Exercício 09: Encontre os 10 primeiros anos bissextos.
-}
exercicio09 :: [Int]
exercicio09 = take 10 exercicio08

{-
Exercício 09: Encontre os 10 últimos anos bissextos
-}
exercicio09b :: [Int]
exercicio09b = drop (length exercicio08 - 10) exercicio08

{-
Exercício 10: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.
-}
exercicio10 :: ([Int], [Int])
exercicio10 = (take half exercicio08 , drop half exercicio08)
    where half = div (length exercicio08) 2
    
{-
Exercício 11: Crie um concatenador de strings que concatena duas strings separadas por espaço.
-}
exercicio11 :: String -> String -> String
exercicio11 a b = a ++ ' ':b

{-
Exercício 12: Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer.
-}
exercicio12 :: String -> [Integer]
exercicio12 = map (toInteger . digitToInt)