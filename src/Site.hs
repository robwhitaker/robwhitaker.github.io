{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

import           Data.Monoid (mappend)
import           Hakyll
import           Data.List (isPrefixOf, isSuffixOf, intercalate)
import           Data.Char (isDigit)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Calendar (toGregorian)
import           System.FilePath.Posix (replaceExtension, takeDirectory, takeBaseName, (</>))
import qualified GHC.IO.Encoding as E
import qualified System.Process as Process

import           Site.Config

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    (year',_,_) <- getCurrentTime >>= return . toGregorian . utctDay
    let ?year = show year'
    hakyll $ do
        cats <- buildCategories "posts/**" (fromCapture "category/*/index.html")
        let ?categories = cats

        tagsRules cats $ \cat pattern -> do
            route idRoute
            compile $ postListCompiler pattern "templates/category.html" $
                        constField "title" cat `mappend`
                        constField "isHome" "true"

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
                        >>= applyAsTemplate baseCtx
                        >>= loadAndApplyTemplate "templates/default.html" baseCtx
                        >>= cleanupUrls

        match "posts/**" $ do
            route $ megaRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= saveSnapshot "postSnapshot"
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= cleanupUrls

        -- TODO: Uncomment once resume is added
        -- match "static/resume/resume.tex" $ do
        --     route   $ constRoute "downloads/robertjwhitaker-resume.pdf"
        --     compile $ xelatex

        create ["index.html"] $ do
            route   $ idRoute
            compile $ postListCompiler "posts/**" "templates/post-list.html" $
                        constField "title" "Home"  `mappend`
                        constField "isHome" "true"

        create ["feed.xml"] $ do
            route   $ idRoute
            compile $ do
                posts <- fmap (take 10) $ recentFirst =<< loadAllSnapshots "posts/**" "postSnapshot"
                renderRss
                    (FeedConfiguration -- TODO: move this into config w/ real info
                        "robwhitaker.com blog"
                        "A simple RSS feed for the blog"
                        "Rob Whitaker"
                        "robjameswhitaker@gmail.com"
                        "http://robwhitaker.com")
                    postCtx
                    posts



        match "templates/*" $ compile templateBodyCompiler


--------- COMPILERS -----------
postListCompiler :: (?categories :: Tags, ?year :: String) => Pattern -> Identifier -> Context String -> Compiler (Item String)
postListCompiler pattern template ctxAddon = do
    posts <- recentFirst =<< loadAll pattern
    let ctx =
            listField "posts" postCtx (return posts) `mappend`
            ctxAddon                                 `mappend`
            baseCtx

    makeItem ""
        >>= loadAndApplyTemplate template ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanupUrls

---------- CONTEXTS -----------
postCtx :: (?categories :: Tags, ?year :: String) => Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "isPost" "true" `mappend`
    categoryField "category" ?categories `mappend`
    mapContext cleanIndex (urlField "url") `mappend` -- override "url" field in baseCtx with cleaned up URL
    baseCtx

baseCtx :: (?categories :: Tags, ?year :: String) => Context String
baseCtx =
    constField "year" ?year `mappend`
    categoriesCtx           `mappend`
    defaultContext

categoriesCtx :: (?categories :: Tags) => Context String
categoriesCtx = field "categories" $ \_ -> renderTags
    (\tag url count _ _ -> concat [ "<a href=\"", url, "\">", tag, "(", show count, ")</a>" ])
    (intercalate " · ")
    ?categories


----------- ROUTES ------------
prettyRoute :: Routes
prettyRoute = customRoute $ \ident ->
    let path = toFilePath ident
        baseName = takeBaseName path
    in
        if baseName /= "index"
            then takeDirectory path </> stripDate baseName </> "index.html"
            else path
  where stripDate :: FilePath -> FilePath
        stripDate path@(y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:'-':rest) =
            if all isDigit (y1:y2:y3:y4:m1:m2:d1:d2:[])
                then rest
                else path
        stripDate path = path

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