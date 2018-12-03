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
import Style

template :: Html -> Html
template body = docTypeHtml $ do
    H.head $ do
        H.title "Knapsack problem - Genetic algorithm"
        H.style $ preEscapedText $ toStrict $ stylesheetToText
        link ! rel "icon" ! href "https://i.imgur.com/xd2jYkZ.png"
    H.body $ do
        body

knapsack :: Html
knapsack = H.img ! class_ "center-img" ! src "https://i.imgur.com/xd2jYkZ.png" ! height "150"

form :: Html
form = do
    H.form ! class_ "form center" ! method "POST" ! action "run" $ do
        textInput "bagSize" "Bag size"
        itemInput "items" "Items"
        textInput "populationSize" "Population size"
        textInput "generations" "Number of generations"
        input ! class_ "button" ! type_ "submit" ! A.value "Run!"

inputPage :: Html
inputPage = do
    h2 "Knapsack problem" ! class_ "center"
    h3 "A genetic algorithm approach" ! class_ "center"
    knapsack
    Main.form

results :: Text -> Text -> Text -> Text -> Result -> Html
results bagSize items populationSize generations result = do
    h2 "Results" ! class_ "center"
    knapsack
    formatSpan "Bag size: " bagSize
    formatSpan "Items: " items
    formatSpan "Population size: " populationSize
    formatSpan "Number of generations: " generations
    H.div ! class_ "center" $ do
        H.span "Solution: " ! class_ "bold"
        H.span $ toHtml $ show result

textInput :: AttributeValue -> AttributeValue -> Html
textInput n p = do
    input ! class_ "input" ! type_ "text" ! name n ! placeholder p
    br

itemInput :: AttributeValue -> AttributeValue -> Html
itemInput n p = do
    input ! class_ "input" ! type_ "text" ! name n ! placeholder p
    H.p "The items input format is an array of (weight, value) tuples."
    H.p "For example: [(10, 4), (5, 3), (10, 6)]"
    br


formatSpan :: Html -> Data.Text.Lazy.Text -> Html
formatSpan title variable = do
    H.div ! class_ "center bottom-margin" $ do
        H.span title ! class_ "bold"
        H.span $ preEscapedText $ toStrict $ variable
        br

runAlgorithm :: Int -> [(Int, Int)] -> Int -> Int -> Result
runAlgorithm bagSize items populationSize generations = chromosomeToResult solution convertedItems
    where randomGen = mkStdGen 1231232
          tupleToItems (x, y) = Item x y
          convertedItems = Prelude.map tupleToItems items
          population = startPopulation populationSize bagSize convertedItems randomGen
          solution = getBestFitness $ runGA population generations bagSize convertedItems randomGen

render = S.html . renderHtml
convert = read . unpack

main :: IO()
main = scotty 3000 $ do
    middleware logStdoutDev

    get "/" $ render $ template $ inputPage

    post "/run" $ do
        bagSize <- S.param "bagSize"
        items <- S.param "items"
        populationSize <- S.param "populationSize"
        generations <- S.param "generations"
        let result = runAlgorithm (convert bagSize) (read $ unpack items :: [(Int, Int)]) (convert populationSize) (convert generations)
        render $ template $ results bagSize items populationSize generations result
