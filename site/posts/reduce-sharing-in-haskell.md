---
title: "3 ways to reduce sharing in Haskell"
author: Yair Chuchem
date: 2022.09.27
tags: [code, haskell, lazy-evaluation, space-leaks]
description: 3 ways to reduce sharing and avoid space leaks in Hasekll
image: planet-catcher.jpg
---

Haskell [is prone to space leaks](/posts/a-simple-challenge-for-haskellers), but luckily there are several ways to work around them.

A simple example for a space leak is iterating over the fibonacci sequence twice, [as described in the previous post](/posts/a-simple-challenge-for-haskellers).

Luckily, several solutions to this problem were suggested by [r/haskell](https://www.reddit.com/r/haskell/comments/xngj2w/a_simple_challenge_for_haskellers/):

## Using INLINE pragmas

[oberblastmeister suggested](https://www.reddit.com/r/haskell/comments/xngj2w/a_simple_challenge_for_haskellers/ipu5qoj/) inlining:

```Haskell
fibs = map fst (iterate (\(cur, next) -> (next, cur + next)) (1, 1))
{-# INLINE fibs #-}
```

This will make separate iterations be independent from each other, each using their own copy of the list which is iterated once and is immediately collected by the GC.

Pros:

* Require minimal changes to our code

Cons:

* Fragile? [kuribas wrote](https://www.reddit.com/r/haskell/comments/xngj2w/a_simple_challenge_for_haskellers/ipxydqx/) that "INLINE doen’t guarantee inlining, and it depends on optimization flags"
* Machine code duplication. May create larger and potentially less efficient executables

## Using the `-fno-full-laziness` GHC flag

[MorrowM_ suggested](https://www.reddit.com/r/haskell/comments/xngj2w/a_simple_challenge_for_haskellers/ipug4yp/) using the `-fno-full-laziness` after wrapping `fibs` with a lambda:

```Haskell
{-# OPTIONS -fno-full-laziness #-}

fibs _ = map fst (iterate (\(cur, next) -> (next, cur + next)) (1, 1))
```

Note that adding the dummy lambda alone will suffice if not compiling with optimizations, but that's not something that we would like to rely on, hence the additional `-fno-full-laziness` option comes into place.

Without it, GHC may notice that the expression in `fibs` doesn't use its parameter and so it may float it out and shares it.

Pros:

* No machine code duplication: the implementation of `fibs` is shared without the resulting list being shared!

Cons:

* Fragile? I fear that this soltuion might break if another module uses `fibs` and GHC decides to inline it, and this second module didn't enable `-fno-full-laziness`
* Relies on GHC flags. These might change more easily than the language standard does
* Requires modification to our code including in all of `fibs`'s call sites

## Functionalization

Alonzo Church famously [discovered that data can be encoded in functions](https://en.wikipedia.org/wiki/Church_encoding), and we can use it to avoid creating data structures that could be shared.

As the Fibonacci sequence is infinite we'll use a function encoding for infinite lists:

```Haskell
fibs :: (Integer -> a -> a) -> a
fibs cons =
    go 1 1
    where
        go cur next =
            cons cur (go next (cur + next))
```

We can convert it to a list using `fibs (:)` but to avoid sharing we'll have to change our code using it, and to solve the problem in the previous post we'll use a custom variant of `find`:

```Haskell
findInf :: (a -> Bool) -> ((a -> a -> a) -> a) -> a
findInf pred infList =
    infList f
    where
        f x rest
            | pred x = x
            | otherwise = rest
```

Btw, [`yeled_kafot` suggested](https://www.reddit.com/r/haskell/comments/xngj2w/a_simple_challenge_for_haskellers/ipvm3g3/) a formulation based on `lens`, which might be of use to folks using this eco-systems.

Pros:

* Solid. Little doubt that this will resume working in the face of various compiler optimization

Cons:

* More verbose
* Non-standard. We're not using standard lists anymore, and those have a wealth of standard library functions supporting them which we may need to re-implement

## Notes

* Image generated using DALL-E 2 with the prompt "A ranger catching a planet with a lasso, digital art". No Lassos there but it's good enough :)
* Do you have another solution to the problem? I'd love to hear about it!
