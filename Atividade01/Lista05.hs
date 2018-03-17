-- Exercícios sobre ADT
module Lista05 where

import Data.List (find)

{-
Exercício 01: Resolva o problema da Zebra utilizando ADTs para representar as soluções. Para encontrar a resposta você deve enumerar todas as combinações até que encontre uma que atenda todas as restrições.
-}

data Color = Yellow | Blue | Red | Ivory | Green deriving (Enum, Show, Eq) -- had to Google "Ivory color" to see what it looks like 
data Nationality = Norwegian | Ukrainian | Englishman | Spaniard | Japanese deriving (Enum, Show, Eq)
data Drink = Water | Tea | Milk | OrangeJuice | Coffee deriving (Enum, Show, Eq)
data Smoke = Kools | Chesterfield | OldGold | LuckyStrike | Parliament deriving (Enum, Show, Eq)
data Pet = Fox | Horse | Snails | Dog | Zebra deriving (Enum, Show, Eq) -- who in seven hells has a fox, snail or a ZEBRA for a pet??

data House = House {color :: Color, nationality :: Nationality, drink :: Drink, smoke :: Smoke, pet :: Pet} deriving Show --record syntax to the rescue!
type Solution = (House, House, House, House, House) -- 5 Houses

allPossibleHouses :: [House]
allPossibleHouses = [House c n d s p | c <- [Yellow ..], n <- [Norwegian ..], d <- [Water ..], s <- [Kools ..], p <- [Fox ..]] --Enum enables us to do this :D

allPossibleSolutions :: [Solution]
allPossibleSolutions = [(a,b,c,d,e) | a <- allPossibleHouses, b <- allPossibleHouses, c <- allPossibleHouses, d <- allPossibleHouses, e <- allPossibleHouses]

trueSolution :: Solution
trueSolution = (
    House Yellow Norwegian Water Kools Fox,
    House Blue Ukrainian Tea Chesterfield Horse,
    House Red Englishman Milk OldGold Snails,
    House Ivory Spaniard OrangeJuice LuckyStrike Dog,
    House Green Japanese Coffee Parliament Zebra
    )

randomSolution :: Solution
randomSolution = (one, two, three, four, five) where [one, two, three, four, five] = take 5 allPossibleHouses

-- in a list
allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

differentAcrossAll :: Eq a => (House -> a) -> Solution -> Bool-- Guarantees that the functions returns differently for all Houses in a solution
differentAcrossAll fn (fir, sec, thi, fou, fiv) = allDifferent $ map fn [fir, sec, thi, fou, fiv]


-- not to be mistaken with contramap
smap :: (House -> a) -> Solution -> [a] -- "maps" a function to our solution
smap fn (one, two, three, four, five) = [fn one, fn two, fn three, fn four, fn five]


-- Now we can play :D
type Condition = Solution -> Bool


validSolution :: Condition -- guarantees that all parameters are different in each Houses of our solution
validSolution s = differentAcrossAll color s && 
                    differentAcrossAll nationality s && 
                    differentAcrossAll drink s && 
                    differentAcrossAll smoke s && 
                    differentAcrossAll pet s

-- The Englishman lives in the red house.
c1 :: Condition
c1 = or . (smap englishRed)
    where englishRed c = nationality c == Englishman && color c == Red
    
-- The Spaniard owns the dog.
c2 :: Condition
c2 = or . (smap spaniardDog)
    where spaniardDog c = nationality c == Spaniard && pet c == Dog
    
-- Coffee is drunk in the green house.
c3 :: Condition
c3 = or . (smap coffeeGreen)
    where coffeeGreen c = drink c == Coffee && color c == Green
    
-- The Ukrainian drinks tea.
c4 :: Condition
c4 = or . (smap ukrTea)
    where ukrTea c = nationality c == Ukrainian && drink c == Tea
    
-- The green house is immediately to the right of the ivory house.
c5 :: Condition --this one is ugly :(
c5 (House{color=Ivory}, House{color=Green}, _, _, _) = True 
c5 (_, House{color=Ivory}, House{color=Green}, _, _) = True 
c5 (_, _, House{color=Ivory}, House{color=Green}, _) = True 
c5 (_, _, _, House{color=Ivory}, House{color=Green}) = True
c5 _ = False

-- The Old Gold smoker owns snails.
c6 :: Condition
c6 = or . (smap oldSnails)
    where oldSnails c = smoke c == OldGold && pet c == Snails

-- Kools are smoked in the yellow house.
c7 :: Condition
c7 = or . (smap koolsYellow)
    where koolsYellow c = smoke c == Kools && color c == Yellow

-- Milk is drunk in the middle house.
c8 :: Condition
c8 (_, _, House{drink=Milk}, _, _) = True
c8 _ = False

-- The Norwegian lives in the first house.
c9 :: Condition
c9 (House{nationality=Norwegian}, _, _, _, _) = True
c9 _ = False

-- The man who smokes Chesterfield lives in the house next to the man with the fox.
c10 :: Condition
c10 (House{smoke=Chesterfield}, House{pet=Fox}, _, _, _) = True 
c10 (House{pet=Fox}, House{smoke=Chesterfield}, _, _, _) = True 
c10 (_, House{smoke=Chesterfield}, House{pet=Fox}, _, _) = True 
c10 (_, House{pet=Fox}, House{smoke=Chesterfield}, _, _) = True 
c10 (_, _, House{smoke=Chesterfield}, House{pet=Fox}, _) = True 
c10 (_, _, House{pet=Fox}, House{smoke=Chesterfield}, _) = True 
c10 (_, _, _, House{smoke=Chesterfield}, House{pet=Fox}) = True
c10 (_, _, _, House{pet=Fox}, House{smoke=Chesterfield}) = True
c10 _ = False

-- Kools are smoked in the house next to the house where the horse is kept.
c11 :: Condition
c11 (House{smoke=Kools}, House{pet=Horse}, _, _, _) = True 
c11 (House{pet=Horse}, House{smoke=Kools}, _, _, _) = True 
c11 (_, House{smoke=Kools}, House{pet=Horse}, _, _) = True 
c11 (_, House{pet=Horse}, House{smoke=Kools}, _, _) = True 
c11 (_, _, House{smoke=Kools}, House{pet=Horse}, _) = True 
c11 (_, _, House{pet=Horse}, House{smoke=Kools}, _) = True 
c11 (_, _, _, House{smoke=Kools}, House{pet=Horse}) = True
c11 (_, _, _, House{pet=Horse}, House{smoke=Kools}) = True
c11 _ = False

-- The Lucky Strike smoker drinks orange juice.
c12 :: Condition
c12 = or . (smap luckyOrange)
    where luckyOrange c = smoke c == LuckyStrike && drink c == OrangeJuice

-- The Japanese smokes Parliaments.
c13 :: Condition
c13 = or . (smap luckyOrange)
    where luckyOrange c = smoke c == Parliament  && nationality c == Japanese

-- The Norwegian lives next to the blue house.
c14 :: Condition
c14 (House{nationality=Norwegian}, House{color=Blue}, _, _, _) = True 
c14 (House{color=Blue}, House{nationality=Norwegian}, _, _, _) = True 
c14 (_, House{nationality=Norwegian}, House{color=Blue}, _, _) = True 
c14 (_, House{color=Blue}, House{nationality=Norwegian}, _, _) = True 
c14 (_, _, House{nationality=Norwegian}, House{color=Blue}, _) = True 
c14 (_, _, House{color=Blue}, House{nationality=Norwegian}, _) = True 
c14 (_, _, _, House{nationality=Norwegian}, House{color=Blue}) = True
c14 (_, _, _, House{color=Blue}, House{nationality=Norwegian}) = True
c14 _ = False

allConditions :: [Condition]
allConditions = [validSolution, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14]

testAllConditions :: Condition
testAllConditions s = and [cond s | cond <- allConditions]

-- And finally:

-- Snare roll.......

findSolution :: Maybe Solution -- Maybe it finds it, I love it!
findSolution = find testAllConditions allPossibleSolutions -- TA-DA!