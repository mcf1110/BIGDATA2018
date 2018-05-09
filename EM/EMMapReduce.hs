module EMMapReduce where
import Dados
import Vector
import Data.List (transpose, foldl1', intercalate)

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
type Dataset = ChunksOf [Datapoint]
type Datapoint = Vector Double
type Responsibility = Vector Double -- Each column is a gaussian

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
eStep :: Dataset -> MixtureModel -> ChunksOf [(Datapoint, Responsibility)]
eStep xs mm = pmap (\x -> (x, pointResposibilities mm x)) xs

-- M STEP
-- Takes a datapoint and its responsibilities and returns each cluster partial mean
partialMeans :: (Datapoint, Responsibility) -> [Mean]
partialMeans (x, r) = map (x .*) r

newMeanNkN :: ChunksOf [(Datapoint, Responsibility)] -> ([Mean], Vector Double, Double)
newMeanNkN xrs = (\(m, nk, n) -> (zipWith (./) m nk, nk, n))
        $ mapReduce
            (\(x,r) -> (partialMeans (x,r), r, 1))
            (\(m1, nk1, n1) (m2,nk2, n2) -> (m1 ..+.. m2, nk1 .+. nk2, n1+n2)) xrs

newCovariance :: ChunksOf [(Datapoint, Responsibility)] -> ([Mean], Vector Double, Double) -> [Covariance]
newCovariance xs (means, nks, _) =
    zipWith (../) covSum nks
    where
        distSq u v = sq (u .-. v) where sq x = outerprod x x
        covSum = mapReduce
                    (\(x, r) -> zipWith (*..) r $ map (distSq x) means)
                    (zipWith (..+..))
                    xs


mStep :: ChunksOf [(Datapoint, Responsibility)] -> MixtureModel
mStep xrs = map (\(mean, cov, mix) -> (mix, (mean, cov))) zipped
    where
        (newMus, nks, n) = newMeanNkN xrs
        newPis = map (/n) nks
        newSigmas = newCovariance xrs (newMus, nks, n)
        zipped = zip3 newMus newSigmas newPis



nextIteration :: Dataset -> MixtureModel -> MixtureModel
nextIteration xs mm = mStep $ eStep xs mm

iterateEM :: Dataset -> MixtureModel -> Int -> MixtureModel
iterateEM xs mm t = iterate (nextIteration xs) mm !! t

initializeWithPoints :: Matrix Double -> MixtureModel
initializeWithPoints points = map (\p -> (1/l, (p, identity d))) points
    where
        l = fromIntegral $ length points
        d = length $ head $ points

logLikelihood :: Dataset -> MixtureModel -> Double
logLikelihood xs mm = mapReduce
    (\x -> log . sum $ map (\(mix, gaussian) -> mix * (normal gaussian x)) mm)
    (+)
    xs
