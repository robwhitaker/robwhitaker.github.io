{-# LANGUAGE OverloadedStrings #-}

module Style.Header where

import           Prelude hiding (span)
import           Clay
import qualified Clay.Media as Media
import           Style.Theme

css :: Theme -> Css
css theme = 
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

            query Media.screen [ Media.maxWidth (px 430) ] $ do
                    fontSize (px 26)

            ".small-header" & fontSize (px 26)

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
    