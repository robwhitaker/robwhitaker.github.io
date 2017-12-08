--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

import           Data.Monoid (mappend)
import           Hakyll
import           Data.Char (toLower)
import           System.FilePath.Posix (takeFileName, (</>))
import qualified GHC.IO.Encoding as E

import           Site.Routes
import           Site.Config
import           Site.URL
--------------------------------------------------------------------------------
main :: IO ()
main = do 
    E.setLocaleEncoding E.utf8
    c <- readConfig
    let ?config = c
    hakyll $ do
        match "static/img/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "src/Style.hs" $ do
            route   $ (setExtension "css") `composeRoutes` 
                      (customRoute $ ((</>) "styles" . map toLower . takeFileName .
                                     toFilePath))
            compile $ getResourceString >>= withItemBody (unixFilter "stack runghc" [])

        let pagesPattern = foldl (.&&.) (fromGlob (staticPageDir ?config </> "**")) 
                                $ complement . fromGlob . (</>) (staticPageDir ?config) <$> ignoredPages ?config                       
        match pagesPattern $ do
            route   $ megaRoute
            compile $ do
                    ext <- getUnderlyingExtension
                    let compiler = 
                            case ext of
                                ".html" -> getResourceBody
                                _       -> pandocCompiler
                    
                    compiler
                        >>= applyAsTemplate defaultContext
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= cleanupUrls

        match "posts/*" $ do
            route $ megaRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= cleanupUrls

        create ["blog/index.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Blog"                `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= cleanupUrls


        match "static/pages/index.html" $ do
            route   $ megaRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= cleanupUrls

        match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "isPost" "true" `mappend`
    defaultContext
