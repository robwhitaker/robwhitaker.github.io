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

Tried specifying to GHC to use UTF-8. Sometimes that helps. Not this time. Now I'm upgrading Slack... my version is pretty behind (1.1.2 instead of 1.6.1). Woops.

And after updating Stack to the most recent version... it stil doesn't work. End Day 2.