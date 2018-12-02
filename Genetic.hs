module Genetic (
    Chromosome (),
    Item (..),
    startPopulation
)
    where

import System.Random
import Data.List

data Chromosome = Chromosome {
    gene :: [Int],
    fitness :: Int
} deriving (Show, Eq)

data Item = Item {
    -- name :: String????
    weight :: Int,
    value :: Int
} deriving (Show)

bag = 50
items = [Item 5 10, Item 30 100, Item 10 5, Item 1 10, Item 40 110, Item 45 100]

calcFitness :: [Int] -> Int
calcFitness gene
    | sumWeight > bag = 0
    | otherwise     = sumValue
    where sumWeight = sumItems gene items weight
          sumValue = sumItems gene items value

sumItems :: [Int] -> [Item] -> (Item -> Int) -> Int
sumItems [] [] _ = 0
sumItems (x:xs) (y:ys) f
    | x == 1 = f y + sumItems xs ys f
    | otherwise = sumItems xs ys f

generateChromosome :: RandomGen g => Int -> g -> Chromosome
generateChromosome n randomGen = Chromosome gene fitness
    where gene = take n $ randomRs (0, 1) randomGen
          fitness = calcFitness gene

startPopulation :: RandomGen g => Int -> g -> [Chromosome]
startPopulation 0 _ = []
startPopulation n randomGen = generateChromosome chromosomeSize randomGen : startPopulation (n - 1) randomGen'
    where chromosomeSize = length items
          randomGen' = snd $ next randomGen

getBestFitness :: [Chromosome] -> Chromosome
getBestFitness (x:[]) = x
getBestFitness (x:xs)
    | fx > fm = x
    | otherwise = maxi
    where maxi = getBestFitness xs
          fx = fitness x
          fm = fitness maxi

runGA :: RandomGen g => [Chromosome] -> Int -> g -> [Chromosome]
runGA p 0 _ = p
runGA p n g = runGA newP (n-1) newG
    where newG = snd (next g)
          newP = newPopulation p g

newPopulation :: RandomGen g => [Chromosome] -> g -> [Chromosome]
newPopulation cs randomGen = children ++ rest
    where selectedChromosome = tournament (selectChromosomes cs randomGen)
          rest = cs \\ selectedChromosome
          children = applyMutation n (makeCrossover selectedChromosome) randomGen
          n = length cs `div` 100

applyMutation :: RandomGen g =>  Int -> [Chromosome] -> g -> [Chromosome]
applyMutation 0 cs _ = cs
applyMutation n (x:xs) randomGen = [mutation x randomGen] ++ applyMutation (n-1) xs randomGen

mutation :: RandomGen g => Chromosome -> g -> Chromosome
mutation c randomGen = Chromosome mutated f
    where gn = gene c
          mutated = invertBit gn rnd
          rnd = fst(randomR (0, (length gn)-1) randomGen)
          f = calcFitness mutated

invertBit :: [Int] -> Int -> [Int]
invertBit [] _ = []
invertBit (x:xs) n
    | n == 0 && x == 1 = [0] ++ xs
    | n == 0 && x == 0 = [1] ++ xs
    | otherwise = [x] ++ invertBit xs (n-1)

makeCrossover :: [Chromosome] -> [Chromosome]
makeCrossover [] = []
makeCrossover (c:[]) = [c]
makeCrossover (c1:c2:cs) = crossover c1 c2 ++ makeCrossover cs

crossover :: Chromosome -> Chromosome -> [Chromosome]
crossover x y = [Chromosome c1 f1, Chromosome c2 f2]
    where gx = gene x
          gy = gene y
          i = length gx `div` 3
          c1 = take i gx ++ drop i gy
          c2 = take i gy ++ drop i gx
          f1 = calcFitness c1
          f2 = calcFitness c2

tournament :: [Chromosome] -> [Chromosome]
tournament [] = []
tournament (c:[]) = [c]
tournament (c1:c2:cs)
    | f1 >= f2 = [c1] ++ tournament cs
    | otherwise = [c2] ++ tournament cs
    where f1 = fitness c1
          f2 = fitness c2

selectChromosomes :: RandomGen g => [Chromosome] -> g -> [Chromosome]
selectChromosomes cs randomGen = selectFromList cs i
    where l = length cs
          n = l `div` 3
          i = take n (nub (randomRs (0, (l-1)) randomGen))

selectFromList :: [Chromosome] -> [Int] -> [Chromosome]
selectFromList _ [] = []
selectFromList cs (x:xs) = [cs !! x] ++ selectFromList cs xs
