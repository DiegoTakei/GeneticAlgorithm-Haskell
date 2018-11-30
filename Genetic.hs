module Genetic ()
    where

import System.Random

data Chromosome = Chromosome {
    gene :: [Int],
    fitness :: Int
} deriving (Show)

data Item = Item {
    -- name :: String????
    weight :: Int,
    value :: Int
} deriving (Show)

bag = 50
items = [Item 5 10, Item 30 100, Item 10 5, Item 1 10, Item 40 110, Item 45 100]

crossover :: Chromosome -> Chromosome -> [Chromosome]
crossover x y = [Chromosome c1 f1, Chromosome c2 f2]
    where gx = gene x
          gy = gene y
          i = length gx `div` 3
          c1 = take i gx ++ drop i gy
          c2 = take i gy ++ drop i gx
          f1 = calcFitness c1
          f2 = calcFitness c2

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