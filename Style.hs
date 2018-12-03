{-# LANGUAGE OverloadedStrings #-}

module Style (stylesheetToText)
    where

import Clay
import Clay.Text
import Clay.Background
import Clay.Border
import Clay.Size
import Clay.Geometry
import Clay.Color
import Clay.Font
import Clay.Display
import Data.Text.Lazy (Text)

bodyCss :: Css
bodyCss = body ? do
    fontFamily ["Ubuntu mono"] [serif]

pCss :: Css
pCss = p ? do
    fontSize $ px 13
    color "#656565"
    margin (px 0) (px 0) (px 0) (px 0)

centerClass :: Css
centerClass = element ".center" ? (textAlign $ alignSide sideCenter)

inputClass :: Css
inputClass = element ".input" ? do
    fontFamily ["Ubuntu mono"] [serif]
    fontSize (px 14)
    borderColor "#3333338a"
    borderRadius (px 5) (px 5) (px 5) (px 5)
    padding (px 8) (px 8) (px 8) (px 8)
    width $ pct 20
    marginBottom (px 5)

buttonClass :: Css
buttonClass = element ".button" ? do
    fontFamily ["Ubuntu mono"] [serif]
    fontSize (px 16)
    backgroundColor "#47b9ff80"
    borderColor "#1886ca38"
    borderRadius (px 5) (px 5) (px 5) (px 5)
    padding (px 5) (px 10) (px 5) (px 10)

boldClass :: Css
boldClass = element ".bold" ? fontWeight bold

bottomMarginClass :: Css
bottomMarginClass = element ".bottom-margin" ? marginBottom (px 5)

centerImgClass :: Css
centerImgClass = element ".center-img" ? do
    display block
    marginLeft auto
    marginRight auto
    marginBottom (px 30)

stylesheetToText :: Text
stylesheetToText = render $ do
    bodyCss
    pCss
    centerClass
    inputClass
    buttonClass
    boldClass
    bottomMarginClass
    centerImgClass