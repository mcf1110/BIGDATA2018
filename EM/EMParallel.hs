module EMParallel where
import Dados
import Vector
import Data.List (transpose, foldl1', intercalate)
import Data.List.Split (chunksOf)

type Mean = Vector Double
type Covariance = Matrix Double
type Mixing = Double

type Gaussian = (Mean, Covariance)

normal :: Gaussian -> Vector Double -> Double
normal (mean, cov) x = result
    where 
        d = fromIntegral $ length x :: Double
        cte = 1/((2*pi)**(d/2))
        raizcov = 1/(sqrt $ pseudodet cov)
        distTrans = [x .-. mean] :: Matrix Double
        dist = transpose distTrans
        invcov = inverse cov
        m1 = distTrans *-* invcov
        m2 = m1 *-* dist
        expoente = -(0.5) * (head $ head $ m2)
        result = cte * raizcov * exp expoente

type MixtureModel = [(Mixing, Gaussian)]
type Dataset = Matrix Double
type Responsibility = Vector Double -- Each column is a datapoint

isValid :: MixtureModel -> Bool
isValid mm = sum (map fst mm) == 1

-- Naively nitializes K clusters of D dimensions
initialize :: Int -> Int -> MixtureModel
initialize k d = [(pik, (take d $ repeat kindex , identity d)) | kindex <- [1..(fromIntegral k)]]
    where pik = 1 / fromIntegral k

-- E STEP

-- Takes a mm, a vector x and returns a vector of size K representing the responsibilities of each gaussian
pointResposibilities :: MixtureModel -> Vector Double -> Responsibility
pointResposibilities mm x = z ./ sum z
    where
        z = map (\(mix, gauss) -> mix * normal gauss x ) mm


-- Takes the dataset and a mixture model, and generates a matrix where each row is a gaussian and each column is a point, containing responsibilities
eStep :: Dataset -> MixtureModel -> [Responsibility]
eStep xs mm = transpose $ parmap (pointResposibilities mm) xs

-- M STEP
-- Takes the dataset, and a responsibility row (which represents a gaussian, whose columns are the datapoints)
newMean :: Dataset -> Responsibility -> Double -> Vector Double
newMean xs respRow nk = (./ nk) $ 
                        mapReduce id (.+.) $ 
                        chunksOf 100 $
                        zipWith (.*) xs respRow


newCovariance :: Dataset -> Mean -> Responsibility -> Double -> Covariance
newCovariance xs mean respRow nk = (../ nk) $ 
                                foldl1' (..+..) $ 
                                zipWith (*..) respRow $
                                parmap mprod xs
    where
        distanceToMean x = [x .-. mean] :: Matrix Double --single row
        mprod x = 
            let d = distanceToMean x in 
                (transpose d) *-* d

-- would be the same as average
newMixing :: Vector Double -> Double -> Double
newMixing respRow nk = nk/n
    where 
        n = fromIntegral $ length respRow


reestimate :: Dataset -> Vector Double -> (Mixing, Gaussian)
reestimate xs respRow = (mix, (mean, cov) )
    where
        sumResp = sum respRow
        mean = newMean xs respRow sumResp
        cov = newCovariance xs mean respRow sumResp
        mix = newMixing respRow sumResp

mStep :: Dataset -> [Responsibility] -> MixtureModel
mStep xs resps = parmap (reestimate xs) resps

nextIteration :: Dataset -> MixtureModel -> MixtureModel
nextIteration xs = (mStep xs) . (eStep xs)

iterateEM :: Dataset -> MixtureModel -> Int -> MixtureModel
iterateEM xs mm t = iterate (nextIteration xs) mm !! t

initializeWithPoints :: Matrix Double -> MixtureModel
initializeWithPoints points = map (\p -> (1/l, (p, identity d))) points 
    where 
        l = fromIntegral $ length points
        d = length $ head $ points

logLikelihood :: Dataset -> MixtureModel -> Double
logLikelihood xs mm = sum $ map (\x -> log . sum $ map (\(mix, gaussian) -> mix * (normal gaussian x)) mm) xs