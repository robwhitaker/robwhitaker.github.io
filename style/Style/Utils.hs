{-# LANGUAGE OverloadedStrings #-}

module Style.Utils where

import           Clay
import           Data.Text (Text)

-- SVG
svgColorProp :: Key Text -> Color -> Css
svgColorProp prop c = prop -: (unPlain . unValue . value $ c)

fill :: Color -> Css
fill = svgColorProp "fill"

stroke :: Color -> Css
stroke = svgColorProp "stroke"