{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay
import qualified Data.Text.Lazy.IO as T
import           Style.Theme (Theme, lightTheme)
import           Style.Stylesheet (stylesheet)

printCss :: Theme -> Bool -> (Theme -> Css) -> IO ()
printCss theme printCompact = 
    if printCompact 
        then T.putStrLn . renderWith compact [] . ($ theme)
        else putCss . ($ theme)

main :: IO ()
main = printCss lightTheme False stylesheet
    

