---
title: "Python's print in Haskell"
author: Yair Chuchem
date: 2022.07.28
tags: [code, python, haskell]
description: Implementing Python's print function in Haskell
image: serpent-and-eve.jpg
---

Python's `print` function doesn't quote strings like Haskell's does. Suppose you wanted that:

```Haskell
main = do
    pyPrint "Hello!" -- Prints Hello! (without quotes)
    pyPrint (1, "a") -- Prints (1,"a")
```

This can be implemented without overlapping instances as follows:

```Haskell
{-# LANGUAGE FlexibleInstances, UndecidableInstances, DataKinds, TypeFamilies #-}

import Data.Proxy

pyPrint :: PyStr a => a -> IO ()
pyPrint = putStrLn . pyStr

class PyStr a where pyStr :: a -> String

type family IsStr a where
    IsStr String = True
    IsStr _ = False

class PyStrHelper a where pyStrHelper :: a -> String
instance PyStrHelper (Proxy True, String) where pyStrHelper = snd
instance Show a => PyStrHelper (Proxy False, a) where pyStrHelper = show . snd

instance PyStrHelper (Proxy (IsStr a), a) => PyStr a where
    pyStr = pyStrHelper . ((,) Proxy :: a -> (Proxy (IsStr a), a))
```

## Good idea or not

I'm writing this post because I'm currently considering a similar solution for processing the types AST of Lamdu.

This AST is parameterized on whether we're displaying an inferred type, which doesn't have edit and refactoring actions available, or whether it is a user-editable type like the definition of a nominal type.

Lamdu's [name-presentation AST pass](https://github.com/lamdu/lamdu/blob/master/doc/Names.md) should have instances that work on both variants and this technique appears to allow that.

What do you think? Is this a good idea?

## Notes

* See more detailed post on this technique [by Kwang](https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html)
* Header image credit: [jw.org](https://www.jw.org/en/library/magazines/watchtower-study-december-2019/did-satan-promise-eve-immortality/)
* Discussion: * Discussions: <img src="/images/reddit.svg" alt="reddit" style="width: 20px; display: inline;"/> [r/haskell](https://www.reddit.com/r/haskell/comments/wa6wml/pythons_print_in_haskell/)
