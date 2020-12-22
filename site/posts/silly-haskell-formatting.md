---
title: "Silly Haskell code formatting is prevalent"
author: Yair Chuchem
date: 2020.12.21
tags: [code, haskell, formatting]
description: The sore state of formatting in Haskell
image: pokemon-4784546_640.png
---

Haskell code in the wild could typically have type signatures formatted like this:

```Haskell
sortByM
    :: Monad m
    => (a -> a -> m Bool)
    -> [a]
    -> m [a]
```

From now on I will call this "silly formatting", to contrast with the proper way to split the same type signature to multiple lines:

```Haskell
sortByM ::
    Monad m =>
    (a -> a -> m Bool) ->
    [a] ->
    m [a]
```

With proper formatting the context and parameters take whole lines. We could easily tell which part is the context, which are parameters, and which is the function output.

Had we wanted to add or remove parameters or the context, with silly formatting we would need to make edits from the middle of one line to the middle of another. If you are interested in collaboration, silly formatting will hit you with spurious merge conflicts!

What would be the silly way to format function bodies? Here:

```Haskell
sortByM _         []     = pure []
sortByM predicate (x:xs) = partitionM (predicate x) xs >>=
                           \(post, pre) -> sortByM predicate pre <> ((x :) <$> sortByM predicate post)
```

Note that to read this snippet you may have needed scroll horizontally - silly, isn't it?

Let's imagine that we wanted to rename `predicate` to `pred` or to `p`. With silly formatting we would also need to modify all of the lines in the function body! That's more work and yet again more work due to unnecessary merge conflicts!

But there's more: silly formatting is difficult to read! Subjectively it's much easier to get used to scanning the text aligned to the left, rather than different silly alignment every time, often far to the right side of the screen or beyond!

If we sacrifice the fancy custom alignments and just indent blocks with simple and consistent 4 spaces,
it may cost us an extra line of code but the benefit in easy maintainability and readabilty is worth it in my opinion:

```Haskell
sortByM _ [] = pure []
sortByM predicate (x:xs) =
    partitionM (predicate x) xs >>=
    \(post, pre) -> sortByM predicate pre <> ((x :) <$> sortByM predicate post)
```

## What drove me to write this post

The Haskell experience has recently gotten much better with [haskell-language-server](https://github.com/haskell/haskell-language-server) and its VS Code integration.

The remaining aspect where my Haskell experience was lacking in comparison to C++ was auto-formatting, where I enjoy letting `clang-format` auto-format my code. I was looking to see what formatting tools exist for Haskell, and discovered that while several tools exist, none fit my taste, and some of these tools were disqualified on the grounds of applying silly formatting.

Among the differenct formatting tools, I'd like to praise [`ormolu`](https://github.com/tweag/ormolu) (and `fourmolu`) for applying more sensible formatting than others.

## Notes

* GHC's source code has a mixture of styles, with higher prevalence for silly style.
* The short type signature of `sortByM` may typically not be split over several lines.
  I used it as a simple example to keep the post short.
* Discussions: <img src="/images/reddit.svg" alt="reddit" style="width: 20px; display: inline;"/> [r/haskell](https://www.reddit.com/r/haskell/comments/khqs1f/silly_haskell_code_formatting_is_prevalent/) mostly thought that this post is the one being silly, and in <img src="/images/twitter-logo.png" alt="twitter" style="width: 20px; display: inline;"/> [Twitter](https://twitter.com/yairchu/status/1341078315155730432) the sentiment was more positive :)
* [Image](https://pixabay.com/illustrations/pokemon-monster-creature-pink-ugly-4784546/) by [LillyCantabile](https://pixabay.com/users/lillycantabile-8561101/?utm_source=link-attribution&utm_medium=referral&utm_campaign=image&utm_content=4784546) from Pixabay

## Appendix

### An alternative implementation of sortByM

```Haskell
sortByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
sortByM p =
    \case
    [] -> pure []
    (x:xs) ->
        partitionM (p x) xs
        >>= both (sortByM p)
        <&> \(post, pre) -> pre <> x : post
```

`\case` lets us avoid a repetition of the name `sortByM`. Had we wanted to rename it, we'd touch less lines of code. Personally I see this as a benefit!
