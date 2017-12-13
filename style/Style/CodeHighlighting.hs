{-# LANGUAGE OverloadedStrings #-}

module Style.CodeHighlighting where

import Clay
import Style.Theme

css :: CodeTheme -> Css
css cTheme =     
    pre ? do
        backgroundColor (bgColor cTheme)
        color (textColor cTheme)
        padding (em 1) (em 1) (em 1) (em 1)
        overflow auto
        
        code ? do
            fontSize (px 18)
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