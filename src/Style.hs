{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude hiding (div)
import           Clay
import qualified Data.Text.Lazy.IO as T

printCss :: Bool -> Css -> IO ()
printCss printCompact = 
    if printCompact 
        then T.putStrLn . renderWith compact []
        else putCss

main :: IO ()
main = printCss False $ do
    body ? do
        color black
        fontSize (px 16)
        margin (px 0) auto (px 0) auto
        width (px 600)
    
    h1 ? fontSize (px 24)
    
    h2 ? fontSize (px 20)

    div # "#header" ? do
        borderBottom solid (px 2) black
        marginBottom (px 30)
        padding (px 12) (px 0) (px 12) (px 0)

        "#navigation" ? do
            textAlign (alignSide sideRight)

            a ? do
                color black
                fontSize (px 18)
                fontWeight bold
                marginLeft (px 12)
                textDecoration none
                textTransform uppercase
    
    div # "#logo" |> a ? do
        color black
        float floatLeft
        fontSize (px 18)
        fontWeight bold
        textDecoration none

    div # "#footer" ? do
        borderTop solid (px 2) black
        color (rgb 85 85 85)
        fontSize (px 12)
        marginTop (px 30)
        padding (px 12) (px 0) (px 12) (px 0)
        textAlign (alignSide sideRight)
        
    div # "#info" ? do
        color (rgb 85 85 85)
        fontSize (px 14)
        fontStyle italic

        
