{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude hiding (div, span)

import           Clay
import qualified Clay.Media as Media
import qualified Clay.Flexbox as FB
import           Data.Text (Text)
import qualified Data.Text.Lazy.IO as T
import           Data.Monoid ((<>))

import           Style.Theme

printCss :: Theme -> Bool -> (Theme -> Css) -> IO ()
printCss theme printCompact =
    if printCompact
        then T.putStrLn . renderWith compact [] . ($ theme)
        else putCss . ($ theme)

main :: IO ()
main = printCss lightTheme False stylesheet

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

        headerCss theme
        footerCss theme
        codeHighlightingCss (codeTheme theme)
        postList theme

        a ? do
            color (linkColor theme)
            fontWeight bold
            textDecoration none

            hover & textDecoration underline

        p ? lineHeight (pct 150)

        span # ".no-space" ? fontSize (px 0)
        span # ".no-br" ? whiteSpace nowrap
        code ? fontSize (px 16)

        (h1 <> h2 <> h3 <> h4 <> h5 <> h6) ? do
            color (highlightColor theme)
            fontWeight normal
            margin (em 0.3) (em 0) (em 0.3) (em 0)

        h1 ? do
            fontSize (px 36)

        h2 ? do
            fontSize (px 32)
            ".big" & fontSize (px 36)
        h3 ? fontSize (px 28)

        (h1 # ".post-title" <> h1 # ".page-title") ? do
            color black
            fontWeight bold
            margin (em 0.2) (em 0) (em 0) (em 0)

        h1 # ".post-title" ? fontSize (px 50)

        ".post-info" ? do
            fontStyle italic
            fontSize (px 17)
            margin (em 0.25) (em 0) (em 1) (em 0)

postList :: Theme -> Css
postList theme =
    ul # ".post-list" ? do
        listStyleType none
        paddingLeft (px 0)

        li ? do
            margin (em 1) (em 0) (em 1) (em 0)

            a # ".post-title" ? fontSize (px 28)

headerCss :: Theme -> Css
headerCss theme =
    header ? do
        fontFamily ["Inconsolata"] [monospace]
        fontSize (px 20)
        marginTop (em 0.2)
        lineHeight (pct 150)

        ".kw" ? do
            color (highlightColor theme)
            fontWeight bolder

        h1 # ".name-logo" ? do
            fontSize (px 34)
            fontWeight bold
            color (mainTextColor theme)
            display inlineBlock
            margin (px 0) (px 0) (px 0) (px 0)

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

footerCss :: Theme -> Css
footerCss theme =
    footer ? do
        marginTop (em 1)
        borderTop solid (px 1) (dividerColor theme)
        padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        textAlign center
        fontSize (px 14)
        clear both

        p ? marginTop (px 3)

        ".social-buttons" ? do
            a ? margin (px 5) (px 5) (px 5) (px 5)

            a |> i ? do
                color (mainTextColor theme)
                fontSize (px 18)
                cursor pointer

                hover & color (linkColor theme)

            a # ".ello-icon" ? do
                display inlineBlock
                position relative
                cursor pointer

                svg ? do
                    display block
                    position absolute
                    top (px 0)
                    left (px 0)
                    bottom (px 0)
                    right (px 0)

                ".ello-circle" ? do
                    fill $ mainTextColor theme

                ".ello-smile" ? do
                    stroke $ containerBgColor theme

                hover & do
                    textDecoration none

                    ".ello-circle" ? do
                        fill $ linkColor theme

                i ? visibility hidden

codeHighlightingCss :: CodeTheme -> Css
codeHighlightingCss cTheme =
    pre ? do
        backgroundColor (bgColor cTheme)
        color (textColor cTheme)
        padding (em 1) (em 1) (em 1) (em 1)
        overflow auto

        code ? do
            fontSize (px 18)
            fontFamily ["Inconsolata"] [monospace]

            a ? do
                color inherit
                fontWeight inherit

                hover & textDecoration none

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

---------- HELPERS ----------
svgColorProp :: Key Text -> Color -> Css
svgColorProp prop c = prop -: (unPlain . unValue . value $ c)

fill :: Color -> Css
fill = svgColorProp "fill"

stroke :: Color -> Css
stroke = svgColorProp "stroke"