--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

import           Data.Monoid (mappend)
import           Hakyll
import           Data.Char (toLower)
import           System.FilePath.Posix ((</>))
import           System.FilePath (replaceExtension, takeDirectory, takeFileName)
import qualified GHC.IO.Encoding as E
import qualified System.Process as Process

import           Site.Routes
import           Site.Config
import           Site.URL
import           Site.Pandoc
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

        match (fromGlob (styleDir ?config </> "**")) $ do
            route   $ constRoute "styles/style.css"
            compile $ unixFilter "stack" ["runghc", "--cwd", styleDir ?config, styleMain ?config] "" >>= makeItem

        let pagesPattern = foldl (.&&.) (fromGlob (staticPageDir ?config </> "**")) 
                                $ complement . fromGlob . (</>) (staticPageDir ?config) <$> ignoredPages ?config                       
        match pagesPattern $ do
            route   $ megaRoute
            compile $ do
                    ext <- getUnderlyingExtension
                    let compiler = 
                            case ext of
                                ".html" -> getResourceBody
                                _       -> customPandocCompiler
                    
                    compiler
                        >>= applyAsTemplate defaultContext
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= cleanupUrls

        match "posts/*" $ do
            route $ megaRoute
            compile $ customPandocCompiler
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

        match "static/resume/resume.tex" $ do
            route   $ constRoute "downloads/robertjwhitaker-resume.pdf"
            compile $ xelatex

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

---------------------------------------------------------------------------------

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