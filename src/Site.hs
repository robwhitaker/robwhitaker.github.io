--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Char (toLower)
import           System.FilePath (takeFileName, (</>))

--------------------------------------------------------------------------------
main :: IO ()
main = do 
    putStrLn "WOW!"
    hakyll $ do
        match "static/img/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "src/Style.hs" $ do
            route   $ (setExtension "css") `composeRoutes` (customRoute $ ((</>) "styles" . map toLower . takeFileName . toFilePath))
            compile $ getResourceString >>= withItemBody (unixFilter "stack runghc" [])

        match (fromList $ map (fromFilePath . ("static/pages/"++) . toFilePath) ["about.rst", "contact.markdown"]) $ do
            route   $ composeRoutes (setExtension "html") (customRoute $ takeFileName . toFilePath)
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls


        match "static/pages/index.html" $ do
            route (customRoute $ takeFileName . toFilePath)
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
