{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude hiding (div, span)
import           Clay
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy.IO as T

data Theme = Theme
    { bodyBgColor :: Color
    , containerBgColor :: Color
    , linkColor :: Color
    , highlightColor :: Color
    , mainTextColor :: Color
    , dividerColor :: Color
    , codeTheme :: CodeTheme
    }

data CodeTheme = CodeTheme
    { bgColor   :: Color
    , kwColor   :: Color
    , cfColor   :: Color
    , dtColor   :: Color
    , fuColor   :: Color
    , stColor   :: Color
    , dvColor   :: Color
    , otColor   :: Color
    , atColor   :: Color
    , coColor   :: Color
    , vaColor   :: Color
    , textColor :: Color        
    }

lightTheme :: Theme
lightTheme = Theme
    { bodyBgColor      = white
    , containerBgColor = white
    , linkColor        = darkred
    , highlightColor   = cornflowerblue
    , mainTextColor    = black
    , dividerColor     = rgb 100 100 100
    , codeTheme = CodeTheme
        { bgColor   = rgb 50 50 50
        , kwColor   = deeppink
        , cfColor   = deeppink
        , dtColor   = skyblue
        , fuColor   = cornflowerblue
        , stColor   = khaki
        , dvColor   = fuchsia
        , otColor   = springgreen
        , atColor   = springgreen
        , coColor   = gray
        , vaColor   = springgreen
        , textColor = white
        }
    }

printCss :: Theme -> Bool -> (Theme -> Css) -> IO ()
printCss theme printCompact = 
    if printCompact 
        then T.putStrLn . renderWith compact [] . ($ theme)
        else putCss . ($ theme)

main :: IO ()
main = printCss lightTheme False $ \theme -> do
    star ? boxSizing borderBox
    (body <> html) ? do
        color (mainTextColor theme)
        fontSize (px 20)
        fontFamily ["Roboto", "Helvetica"] [sansSerif]
        height (pct 100)
        padding (px 0) (px 0) (px 0) (px 0)
        margin (px 0) (px 0) (px 0) (px 0)
        borderTop solid (px 10) (highlightColor theme)
        backgroundColor (bodyBgColor theme)

    div # "#container" ? do
        margin (px 0) auto (px 0) auto
        maxWidth (px 800)
        minHeight (pct 100)
        padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        backgroundColor (containerBgColor theme)
    
        header ? do
            fontFamily ["Inconsolata"] [monospace]
            fontSize (px 20)
            marginTop (em 0.2)
            lineHeight (pct 150)

            ".kw" ? do
                color (highlightColor theme)
                fontWeight bolder
            
            ".name-logo" ? do
                fontSize (px 34)
                fontWeight bold

                ".small-header" & fontSize (px 23)

            nav ? do
                marginLeft (em 2)

                a ? do
                    color (linkColor theme)
                    fontWeight bold
                    textDecoration none

                    hover & textDecoration underline

                span # "#projects-dropdown" ? do
                    position relative
                    display inlineBlock
                    cursor pointer
                    color (linkColor theme)

                    "#dropdown-items" ? do
                        opacity 0
                        overflow hidden
                        position absolute
                        top (pct 100)
                        right (px (-5))
                        width (px 0)
                        backgroundColor (containerBgColor theme)
                        boxShadow (px 2) (px 5) (px 10) black
                        transitions 
                            [ ("opacity", sec 0.4, ease, sec 0)             
                            , ("width", sec 0.2, ease, sec 0)      
                            ]

                        a ? do
                            display block
                            textAlign center

                    hover & "#dropdown-items" ? do 
                        width (px 120)
                        opacity 100
        
        a ? do
            color (linkColor theme)
            fontWeight bold
            textDecoration none

            hover & textDecoration underline

        p ? lineHeight (pct 150)

        footer ? do
            marginTop (em 1)
            borderTop solid (px 1) (dividerColor theme)
            padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
            textAlign center
            fontSize (px 14)

        span # ".no-space" ? fontSize (px 0)
        
        (h1 <> h2 <> h3 <> h4 <> h5 <> h6) ? do 
            color (highlightColor theme)
            fontWeight normal
        
        h1 ? fontSize (px 30)

        (h1 # ".post-title" <> h1 # ".page-title") ? do
            fontSize (px 50)
            color black
            fontWeight bold
            margin (em 0.2) (em 0) (em 0) (em 0)
        
        h2 ? fontSize (px 24)

        -- CODE HIGHLIGHTING
        let cTheme = codeTheme theme    
        pre ? do
            backgroundColor (bgColor cTheme)
            color (textColor cTheme)
            padding (em 1) (em 1) (em 1) (em 1)
            fontSize (px 18)
            
            code ? do
                fontFamily ["Inconsolata"] [monospace]

                ".kw" ? color (kwColor cTheme)

                ".cf" ? color (cfColor cTheme)

                ".dt" ? color (dtColor cTheme)

                ".fu" ? color (fuColor cTheme)

                ".st" ? color (stColor cTheme)

                ".dv" ? color (dvColor cTheme)

                ".ot" ? color (otColor cTheme)

                ".at" ? color (atColor cTheme)

                ".co" ? color (coColor cTheme)

                ".va" ? do
                    color (vaColor cTheme)
                    fontStyle italic

            
