{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module Site.Routes where

import           Hakyll
import           Site.Config
import           System.FilePath (takeDirectory, takeBaseName, (</>))
import           Data.List (isPrefixOf)

prettyRoute :: Routes
prettyRoute = customRoute $ \ident ->
    let path = toFilePath ident
        baseName = takeBaseName path
    in if baseName /= "index"
            then takeDirectory path </> baseName </> "index.html"
            else path

stripParentDirsRoute :: [FilePath] -> Routes
stripParentDirsRoute pDirs = customRoute $ \ident ->
    let path = toFilePath ident
    in case filter (`isPrefixOf` path) pDirs of
        [] -> path
        (x:_) -> drop (length x) path

megaRoute :: (?config :: Config) => Routes
megaRoute =
    stripParentDirsRoute (stripPrefixDirs ?config) `composeRoutes`
    prettyRoute