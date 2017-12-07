---
title: Contact
---

I live in a small hut in the mountains of Kumano Kodō on Kii Hantō and would not
like to be contacted.

```haskell
module Main (main, taco) where

import Distribution.Simple

data Wow = Wow String

main :: String -> IO ()
main = do
    let wee = "wow"
    let wow = show $ 10 + 13
    defaultMain

{- following code borrowed from Matt Parsons's blog for
 - testing purposes...
 -}
withFile :: FilePath -> IOMode 
    -> (Handle -> IO a) -- callback to lift
    -> IO a

withFileLifted :: (MonadBaseControl IO m, StM m a ~ a)
    => FilePath
    -> IOMode
    -> (Handle -> m a)
    -> m a
withFileLifted path mode action =
    control $ \runInIO ->
        withFile path mode (\handle -> 
            runInIO (action handle))
```

```javascript
function neatoBurrito() {
    [1,2,3].forEach(function(el) {
        console.log(el * 2 + "Some text here");
        return null;
    });
}
```