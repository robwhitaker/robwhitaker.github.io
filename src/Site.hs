{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend)
import           Hakyll
import           Data.Char (toLower)
import           Data.List (isPrefixOf, isSuffixOf)
import           System.FilePath.Posix (replaceExtension, takeDirectory, takeBaseName, takeFileName, (</>))
import qualified GHC.IO.Encoding as E
import qualified System.Process as Process

import           Site.Config

main :: IO ()
main = do 
    E.setLocaleEncoding E.utf8
    hakyll $ do
        match "static/img/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "style/**" $ do
            route   $ constRoute "styles/style.css"
            compile $ unixFilter "stack" ["runghc", "--cwd", "style/", "Style.hs"] "" >>= makeItem

        match "static/pages/**" $ do
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

        -- TODO: Uncomment once resume is added
        -- match "static/resume/resume.tex" $ do
        --     route   $ constRoute "downloads/robertjwhitaker-resume.pdf"
        --     compile $ xelatex

        create ["index.html"] $ do
            route   $ idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        constField "isHome" "true"               `mappend` 
                        defaultContext
                
                makeItem ""
                    >>= loadAndApplyTemplate "templates/post-list.html" indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= cleanupUrls

        match "templates/*" $ compile templateBodyCompiler

---------- CONTEXTS -----------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "isPost" "true" `mappend`
    defaultContext

----------- ROUTES ------------
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

megaRoute :: Routes
megaRoute =
    stripParentDirsRoute ["static/pages/"] `composeRoutes`
    prettyRoute

------------ URLs -------------
withInternalUrls :: (String -> String) -> String -> String
withInternalUrls fn =
    withUrls (\str -> if isExternal str then str else fn str)

    
cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
    where idx = "index.html"

stripIndexUrls :: Item String -> Compiler (Item String)
stripIndexUrls = return . fmap (withInternalUrls cleanIndex)

cleanupUrls :: Item String -> Compiler (Item String)
cleanupUrls item = return item
        >>= stripIndexUrls
        >>= relativizeUrls

saveCleanUrlSnapshot :: Snapshot -> Item String -> Compiler (Item String)
saveCleanUrlSnapshot snapshot item = do
    saveSnapshot snapshot =<< stripIndexUrls item
    return item

---------- LATEX --------------
-- A compiler for tex files based on https://github.com/jaspervdj/jaspervdj/blob/master/src/Main.hs#L261-L275
xelatex :: Compiler (Item TmpFile)
xelatex = do 
    texFilePath <- toFilePath <$> getUnderlying
    texContents <- itemBody <$> getResourceBody
    TmpFile tmpTexPath <- newTmpFile "tmp-tex.tex"
    let sourceDir = takeDirectory texFilePath
        tmpDir    = takeDirectory tmpTexPath
        pdfPath   = replaceExtension tmpTexPath "pdf"
    
    unsafeCompiler $ do
        writeFile tmpTexPath texContents
        _ <- Process.system $ unwords 
            [ "xelatex"
            , "-halt-on-error"
            , "-output-directory", tmpDir
            , "-include-directory", sourceDir
            , tmpTexPath
            , ">/dev/null", "2>&1"
            ]
        return ()
    
    makeItem $ TmpFile pdfPath