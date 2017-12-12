{-# LANGUAGE OverloadedStrings #-}

module Style.Stylesheet where

import           Prelude hiding (div, span)
import           Clay
import           Data.Monoid ((<>))
import           Style.Theme
import qualified Clay.Flexbox as FB
import qualified Style.CodeHighlighting as CodeHighlighting
import qualified Style.Header as Header
import qualified Style.Footer as Footer

stylesheet :: Theme -> Css
stylesheet theme = do
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
        
        Header.css theme
        Footer.css theme 
        CodeHighlighting.css (codeTheme theme)

        a ? do
            color (linkColor theme)
            fontWeight bold
            textDecoration none

            hover & textDecoration underline

        p ? lineHeight (pct 150)

        span # ".no-space" ? fontSize (px 0)
        span # ".no-br" ? whiteSpace nowrap
        
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

        -- FLEXY THINGS

        ".flex-container" ? do
            display flex
            flexWrap FB.wrap
            justifyContent spaceAround
            alignContent flexStart

        ".flex-container" |> star ? do
            FB.flex 1 1 (px 0) 
            margin auto (em 0.5) auto (em 0.5)

            form ? do
                marginTop (px 20)

                (input <> textarea <> button) ? do
                    padding (px 10) (px 10) (px 10) (px 10)
                    fontSize (px 20)
                    display block
                    width (pct 100)
                    marginTop (px 7)
                    border solid (px 1) (dividerColor theme)
                
                textarea ? do
                    minHeight (px 150)

                button ? do
                    background transparent
                    border solid (px 2) (highlightColor theme)
                    color (highlightColor theme)
                    fontWeight bold
                    cursor pointer
                    transition "background" (ms 200) ease (ms 0)

                    hover & background (lighten 0.7 $ highlightColor theme)