{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty as S
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (toStrict, unpack, Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Blaze.Internal (preEscapedText)
import Network.Wai.Middleware.RequestLogger
import Genetic
import System.Random

form :: Html
form = do
    H.form ! method "POST" ! action "run" $ do
        textInput "bagSize" "Bag size"
        textInput "items" "Items"
        textInput "populationSize" "Population size"
        textInput "generations" "Number of generations"
        input ! type_ "submit"

textInput :: AttributeValue -> AttributeValue -> Html
textInput n p = do
    input ! type_ "text" ! name n ! placeholder p
    br

formatSpan :: Html -> Data.Text.Lazy.Text -> Html
formatSpan title variable = do
    H.span title
    H.span $ preEscapedText $ toStrict $ variable
    br

runAlgorithm :: Int -> [(Int, Int)] -> Int -> Int -> Result
runAlgorithm bagSize items populationSize generations = chromosomeToResult $ getBestFitness $ runGA population generations bagSize convertedItems randomGen
    where randomGen = mkStdGen 1231232
          tupleToItems (x, y) = Item x y
          convertedItems = Prelude.map tupleToItems items
          population = startPopulation populationSize bagSize convertedItems randomGen

render = S.html . renderHtml
convert = read . unpack

main :: IO()
main = scotty 3000 $ do
    middleware logStdoutDev

    get "/" $ render $ Main.form

    post "/run" $ do
        bagSize <- S.param "bagSize"
        items <- S.param "items"
        populationSize <- S.param "populationSize"
        generations <- S.param "generations"
        let x = runAlgorithm (convert bagSize) (read $ unpack items :: [(Int, Int)]) (convert populationSize) (convert generations)
        render $ do
            formatSpan "Bag size: " bagSize
            formatSpan "Items: " items
            formatSpan "Population size: " populationSize
            formatSpan "Number of generations: " generations
            H.span "Solution: "
            H.span $ toHtml $ show x
