---
title: Getting Started With Hakyll and Clay
---

Guess I'll take notes... in post form!

# Day 1. Compiling CSS

TL;DR - `stack runghc`, which is recommended to quickly compile Clay, does NOT pay attention to the .cabal file, meaning, unless you're code is in the root directory of the project, it's not about to find your modules. Temporary solution: keep all Clay source in one file, which `runghc` can remarkably figure out.

# Day 2. Contact Form Disaster and Terminal Nightmares

## The contact form

Contact form, contained in `contact.html` and being compiled in the same way as all the other pages, looks great. Except, not really because it doesn't _look_ like anything at all. For some reason, forms are not appearing---everything else is, just not forms... contact forms. The kicker? The forms will appear on any other page, but on this one, they are just not compiled into the project. So, better try to make a new file and see if forms will work there...

## Making another file

```
$ touch static/pages/testfile.html
$ stack exec site build
...
site.exe: _site\.\apple\index.html: commitBuffer: invalid argument (invalid character)
```

... wonderful

Wondering whether it had to do with the order the file was being produced in by the program (no idea how an invalid character is just now appearing), I renamed it a few times, changed the content, etc. etc. No luck. Then, I thought, maybe it's my terminal? Let me just change it through VSCode's rename. Boom. Fixed.

Well, if that's working, then surely...

## The return to contact

Nope.

Still no form. But this time I had an idea: I changed the file type to .md, and whaddayaknow? There's a form field. It just doesn't work as .html, which is, you know, perfectly reasonable. Odd, though, since I can produce a form field on index.html, but that's being compiled somewhat differently so who knows what's going on...?

I'll have to see if I did anything weird later.

## Oh good...

Now I'm getting the commitBuffer message for this file. It's breaking on punctuation... like this line would have already broken it twice.

Tried specifying to GHC to use UTF-8. 

```haskell
import System.IO (hSetEncoding, stdout, utf8)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hakyll $ do
        ...
```

Sometimes that helps. Not this time. Now I'm upgrading Stack... my version is pretty behind (1.1.2 instead of 1.6.1). Woops.

And after updating Stack to the most recent version (this was a many hour process of trying to get `stack upgrade` to work and then just downloading a new .exe which worked immediately---*sigh*)... it still doesn't work. End Day 2.

# Day 3. It works

## commitBuffer: invalid argument (invalid character) REDUX

After some less half-asleep searching, I found the answer issue almost immediately [on the Hakyll website](https://jaspervdj.be/hakyll/tutorials/faq.html#hgetcontents-invalid-argument-or-commitbuffer-invalid-argument). The issue was that Hakyll couldn't write from a UTF-8 encoded file (which is how my VSCode is set to write), so I was on the write track yesterday, just the wrong module, I guess.

```haskell
import qualified GHC.IO.Encoding as E

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyll $ do
        ...
```

Yeah, that's it. There were some other solutions for Windows or Unix, but this one seemed the most portable.

## Back to the contact page

As the FAQ linked above recommends, if Pandoc is eating your HTML, you can try to use the `getResourceBody` compiler. Well, I have it switching over to that now for HTML pages, and it no longer replaces variables in the page. As described, it returns the resource body and doesn't touch your page, which is... not ideal.

Luckily, we can use `applyAsTemplate` to handle all those templating variables in our page. Our page compiler now looks like:

```haskell
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
```

Now our variables are showing up, but more importantly, so is our form! Huzzah!

## A strange little URL issue

I noticed a lot of my URLs had this strange character in them (`%5C`) where a forward slash should be. It didn't seem to mess up the site, but it sure made my URLs look ugly!

`http://localhost:8000/posts%5C2015-12-07-tu-quoque%5C`

vs

`http://localhost:8000/posts/2015-12-07-tu-quoque/`

The forward slash version would get me the page, but it wouldn't be able to reference the stylesheet properly, and besides, why should I even have `%5C` in my URLs anyway? Where did I go wrong?

Well, after looking up `%5C` and realizing it was an escaped backslash, it dawned on me. I was joining together some URLs (or identifiers, in Hakyll-speak) with the `(</>)` operator from `System.FilePath`. The issue here is that I'm on a Windows machine (eww, Windows dev, I know) and that the version of `System.FilePath` I was getting was the Windows one. I changed my imports to `System.FilePath.Posix`, and boom, now I have nice, properly functioning, URLs.