-- Exercícios sobre Listas
module Lista03 where
import Lista02(ehPrimo)
import Data.List (maximumBy)
import Data.Ord(comparing)


{-
Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.
-}
isMultipleOf :: Integer -> Integer -> Bool
isMultipleOf x y = rem y x == 0

divisivel20 :: Integer -> Bool
divisivel20 x = all id [isMultipleOf n x | n <- [1..20]]

exercicio01 = divisivel20

{-
Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.
-}
projectEuler5 :: Integer
projectEuler5 = testNumber 20
    where
    testNumber x
        | divisivel20 x = x
        | otherwise = testNumber (x+1)

-- Na teoria, a multiplicacao de todos os primos e raizes de quadrados perfeitos deve dar o resultado que queremos
fastProjectEuler5 :: Integer
fastProjectEuler5 =  product $ (filter ehPrimo [1..20]) ++ ( map raizMaisProxima (filter quadradoPerfeito [1..20]))
    where
    raizMaisProxima = floor . sqrt . fromIntegral
    quadradoPerfeito n = sq * sq == n
        where sq = raizMaisProxima n

exercicio02 = fastProjectEuler5

{-
Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.
-}
fibonacci :: [Integer]
fibonacci = 1 : 1 : prox 1 1
    where prox x y = x + y : prox y (x+y)

exercicio03 = fibonacci

{-
Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)
-}

projectEuler2 :: Integer
projectEuler2 = sum $ filter even $ takeWhile (<4000000) $ fibonacci

exercicio04 = projectEuler2

{-
Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.
-}

dotProduct :: Num a => [a] -> [a] -> a
dotProduct a b = sum $ zipWith (*) a b

exercicio05 = dotProduct

{-
Exercício 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.
-}
collatz :: Integer -> Integer
collatz x 
    | even x = div x 2
    | otherwise = 3*x + 1

exercicio06 = collatz
{-
Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.
-}
collatzLen :: Integer -> Int
collatzLen x = length $ takeWhile (>1) $ x: prox x
    where prox x = collatz x : (prox (collatz x))

exercicio07 = collatzLen
    
{-
Exercício 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)
-}
projectEuler14 :: Integer
projectEuler14 = maximumBy (comparing collatzLen) [1..1000000]

exercicio08 = projectEuler14

main = do
    putStrLn $ "Exercicio 01: " ++ show (exercicio01 3 )
    putStrLn $ "Exercicio 02: " ++ show (exercicio02)
    putStrLn $ "Exercicio 03 (até o decimo el): " ++ show (take 10 exercicio03)
    putStrLn $ "Exercicio 04: " ++ show (exercicio04)
    putStrLn $ "Exercicio 05: " ++ show (exercicio05 [1,2] [3,4])
    putStrLn $ "Exercicio 06: " ++ show (exercicio06 14)
    putStrLn $ "Exercicio 07: " ++ show (exercicio07 2)
    putStrLn $ "Exercicio 08: " ++ show (exercicio08)