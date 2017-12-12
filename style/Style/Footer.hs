{-# LANGUAGE OverloadedStrings #-}

module Style.Footer where

import Clay
import Style.Theme
import Style.Utils

css :: Theme -> Css
css theme = 
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
