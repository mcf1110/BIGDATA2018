-- Exercícios sobre Matrizes
module Lista04 where
{-
Exercício 01: Faça uma função que gere uma matriz identidade de tamanho n.
-}

type Matriz a = [[a]]

identidade :: Num a => Int -> Matriz a
identidade n = makeId 0 n
    where 
        zeros n = take n $ repeat 0
        makeId index n
            | index >= n = []
            | otherwise = (zeros index ++ [1] ++ zeros (n - index - 1)) : makeId (index +1 ) n

exercicio01 = identidade

{-
Exercício 02: Faça uma função que calcule a soma da diagonal principal de uma matriz.
-}
somaPrincipal :: Num a => Matriz a -> a
somaPrincipal m = sum [m !! i !! i | i <- [0..size] ]
    where size = length m - 1

exercicio02 = somaPrincipal
{-
Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz.
-}
somaSecundaria :: Num a => Matriz a -> a
somaSecundaria m = sum [m !! i !! (size - i) | i <- [0..size] ]
    where size = length m - 1

exercicio03 = somaSecundaria


main = do
    putStrLn $ "Exercicio 01: " ++ show (exercicio01 4)
    putStrLn $ "Exercicio 02: " ++ show (exercicio02 [[1,2,3],[4,5,6],[7,8,9]])
    putStrLn $ "Exercicio 03: " ++ show (exercicio03 [[1,2,3],[4,5,6],[7,8,9]])