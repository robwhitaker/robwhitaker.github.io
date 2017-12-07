module Site.URL where
    
import           Hakyll
import           Data.List (isSuffixOf)

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