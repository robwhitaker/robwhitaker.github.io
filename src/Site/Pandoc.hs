{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (customPandocCompiler) where

import           Hakyll
import           Text.Pandoc
import           Text.Pandoc.Walk (walk)

fixLinkTarget :: Inline -> Inline
fixLinkTarget link@(Link (identifier, classes, pairs) inlines (url, title)) =
        if not (isExternal url)
            then link
            else 
                let newPairs = ("target", "_blank") : filter (\(key, _) -> key /= "target") pairs
                in Link (identifier, classes, newPairs) inlines (url, title)
fixLinkTarget x = x

fixLinkTargetPandoc :: Pandoc -> Pandoc
fixLinkTargetPandoc = walk fixLinkTarget

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions fixLinkTargetPandoc