import Genetic
import System.Random

startProblem :: IO ()
startProblem = do
    putStr("How big is your bag?\n> ")
    bagSize <- getLine
    putStr("Which items are you trying to put in it? (weight and value)\n> ")
    -- talvez dividir isso em mais linhas
    -- por simplicidade no codigo, atualmente o usuario entra um array de tuplas
    rawItems <- getLine
    let items = map tupleToItem (read rawItems)
    startAlgorithm (read bagSize) items

tupleToItem :: (Int, Int) -> Item
tupleToItem (x,y) = Item x y

startAlgorithm :: Int -> [Item] -> IO ()  
startAlgorithm bagSize items = do
    putStr("How big do you want the population to be?\n> ")
    populationSize <- getLine
    putStr("How many generations?\n> ")
    generations <- getLine
    --get Taxa de crossover
    --get Taxa de mutacao
    randomGen <- getStdGen
    let population = startPopulation (read populationSize) randomGen
    executionLoop bagSize items population (read generations)

executionLoop :: Int -> [Item] -> [Chromosome] -> Int -> IO()
executionLoop bagSize items population gen
    | gen == 0 = showResults
    | otherwise = do
        undefined
        -- seleciona individuos para cruzar (imagino que a função de crossover já faça isso)
        -- crossover
        -- mutação
        -- repete

showResults :: IO()
showResults = undefined

main = do
   startProblem 
