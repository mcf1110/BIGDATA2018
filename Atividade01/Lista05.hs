-- Exercícios sobre ADT
module Lista05 where

{-
Exercício 01: Resolva o problema da Zebra utilizando ADTs para representar as soluções. Para encontrar a resposta você deve enumerar todas as combinações até que encontre uma que atenda todas as restrições.
-}

data Color = Yellow | Blue | Red | Ivory | Green deriving (Enum, Show, Eq, Bounded) -- had to Google "Ivory color" to see what it looks like 
data Nationality = Norwegian | Ukrainian | Englishman | Spaniard | Japanese deriving (Enum, Show, Eq, Bounded)
data Drink = Water | Tea | Milk | OrangeJuice | Coffee deriving (Enum, Show, Eq, Bounded)
data Smoke = Kools | Chesterfield | OldGold | LuckyStrike | Parliament deriving (Enum, Show, Eq, Bounded)
data Pet = Fox | Horse | Snails | Dog | Zebra deriving (Enum, Show, Eq, Bounded) -- who in seven hells has a fox, snail or a ZEBRA for a pet??

data House = House {color :: Color, nationality :: Nationality, drink :: Drink, smoke :: Smoke, pet :: Pet} deriving Show --record syntax to the rescue!
type Solution = [House] -- 5 Houses

-- Debugging
trueSolution :: Solution
trueSolution = [
    House Yellow Norwegian Water Kools Fox,
    House Blue Ukrainian Tea Chesterfield Horse,
    House Red Englishman Milk OldGold Snails,
    House Ivory Spaniard OrangeJuice LuckyStrike Dog,
    House Green Japanese Coffee Parliament Zebra
    ]

every :: (Enum a, Bounded a) => [a] -- call it like "every :: [Color]""
every = [minBound..]

allPossibleHouses :: [House] --3125, muito custoso!
allPossibleHouses = [House c n d s p | 
                        c <- every :: [Color], 
                        n <- every :: [Nationality], 
                        d <- every :: [Drink], 
                        s <- every :: [Smoke], 
                        p <- every :: [Pet]]
                        
type HouseCondition = House -> Bool

-- The Englishman lives in the red house.
hc1 :: HouseCondition
hc1 h@(House{nationality=Englishman}) = color h == Red
hc1 _ = True
    
-- The Spaniard owns the dog.
hc2 :: HouseCondition
hc2 h@(House{nationality=Spaniard}) = pet h == Dog
hc2 _ = True
    
-- Coffee is drunk in the green house.
hc3 :: HouseCondition
hc3 h@(House{drink=Coffee}) = color h == Green
hc3 _ = True
    
-- The Ukrainian drinks tea.
hc4 :: HouseCondition
hc4 h@(House{drink=Tea}) = nationality h == Ukrainian
hc4 _ = True

-- The Old Gold smoker owns snails.
hc5 :: HouseCondition
hc5 h@(House{smoke=OldGold}) = pet h == Snails
hc5 _ = True

-- Kools are smoked in the yellow house.
hc6 :: HouseCondition
hc6 h@(House{smoke=Kools}) = color h == Yellow
hc6 _ = True

-- The Lucky Strike smoker drinks orange juice.
hc7 :: HouseCondition
hc7 h@(House{smoke=LuckyStrike}) = drink h == OrangeJuice
hc7 _ = True

-- The Japanese smokes Parliaments.
hc8 :: HouseCondition
hc8 h@(House{smoke=Parliament}) = nationality h == Japanese
hc8 _ = True

isValidHouse :: HouseCondition
isValidHouse h = and $ map ($ h) [hc1, hc2, hc3, hc4, hc5, hc6, hc7, hc8]

allValidHouses :: [House] -- 570 casas, dá pra fazer (:
allValidHouses = filter isValidHouse allPossibleHouses

allPossibleSolutions :: [Solution]
allPossibleSolutions = [[a,b,c,d,e] | a <- allValidHouses, b <- allValidHouses, c <- allValidHouses, d <- allValidHouses, e <- allValidHouses]


-- in a list
isUnique :: (Eq a) => [a] -> Bool
isUnique []     = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

differentAcrossAll :: Eq a => (House -> a) -> Solution -> Bool-- Guarantees that the functions returns differently for all Houses in a solution
differentAcrossAll fn ls = isUnique $ map fn ls

type SolutionCondition = Solution -> Bool

validSolution :: SolutionCondition -- guarantees that all parameters are different in each Houses of our solution
validSolution s = differentAcrossAll color s && 
                    differentAcrossAll nationality s && 
                    differentAcrossAll drink s && 
                    differentAcrossAll smoke s && 
                    differentAcrossAll pet s


-- Now we can play :D

-- The green house is immediately to the right of the ivory house.
sc1 :: SolutionCondition
sc1 ((House{color=Ivory}):(House{color=Green}):_) = True 
sc1 ((House{color=Ivory}):_) = False 
sc1 ((House{color=Green}):_) = False 
sc1 (_:[]) = False
sc1 (_:xs) = sc1 xs


-- Milk is drunk in the middle house.
sc2 :: SolutionCondition
sc2 (_:_:(House{drink=Milk}):_) = True
sc2 _ = False

-- The Norwegian lives in the first house.
sc3 :: SolutionCondition
sc3 ((House{nationality=Norwegian}):_) = True
sc3 _ = False


-- The man who smokes Chesterfield lives in the house next to the man with the fox.
sc4 :: SolutionCondition
sc4 ((House{smoke=Chesterfield}):(House{pet=Fox}):_) = True 
sc4 ((House{pet=Fox}):(House{smoke=Chesterfield}):_) = True 
sc4 ((House{pet=Fox}):_) = False 
sc4 ((House{smoke=Chesterfield}):_) = False 
sc4 (_:[]) = False
sc4 (_:xs) = sc1 xs

-- Kools are smoked in the house next to the house where the horse is kept.
sc5 :: SolutionCondition
sc5 ((House{smoke=Kools}):(House{pet=Horse}):_) = True 
sc5 ((House{pet=Horse}):(House{smoke=Kools}):_) = True 
sc5 ((House{pet=Horse}):_) = False 
sc5 ((House{smoke=Kools}):_) = False 
sc5 (_:[]) = False
sc5 (_:xs) = sc1 xs

-- The Norwegian lives next to the blue house.
sc6 :: SolutionCondition
sc6 ((House{nationality=Norwegian}):(House{color=Blue}):_) = True 
sc6 ((House{color=Blue}):(House{nationality=Norwegian}):_) = True 
sc6 ((House{color=Blue}):_) = False 
sc6 ((House{nationality=Norwegian}):_) = False 
sc6 (_:[]) = False
sc6 (_:xs) = sc1 xs

allConditions :: [SolutionCondition]
allConditions = [validSolution, sc1, sc2, sc3, sc4, sc5, sc6]

testAllConditions :: SolutionCondition
testAllConditions s = and [cond s | cond <- allConditions]

-- And finally:

-- Snare roll.......

findSolution :: Solution
findSolution = head $ filter testAllConditions allPossibleSolutions -- TA-DA!