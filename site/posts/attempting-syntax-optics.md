---
title: "Trying to create Syntax Optics"
author: Yair Chuchem
date: 2020.11.19
tags: [code, declarative, haskell, optics, parsing]
description: Declarative parsing and pretty printing with error reporting
image: camera-parts.png
---

The journey to create combinators for parsing and pretty-pretting continues!

This post (along with the new [`syntax-optics` repo](https://github.com/yairchu/syntax-optics/)) combines two previous efforts:

* ["Elegant AST Parsing and Building with Prisms"](/posts/codecs-as-prisms-asts) declared `Prism`s to parse and print ASTs, but lacking descriptive errors when the parsing fails
* ["Basic error reporting for optics"](/posts/optics-with-error-reporting) declared new `lens`-compatible optics that add error reporting to parse errors

The resulting combinators let us nicely declare syntax:

```Haskell
data Expr
    = Lit Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Show, Eq)
makePrisms ''Expr

expr :: VerbosePrism' String String Expr
expr = tokens . takeExpr . endOfTokens

takeExpr :: VerbosePrism' String [String] (Expr, [String])
takeExpr =
    infixOpLeftRecursion p "+" _Add $           -- Additions of
    infixOpLeftRecursion p "*" _Mul $           -- multiplications of
    tryMatchAtom p (prismFallback _Lit) _Show $ -- literals or
    parens takeExpr                             -- expressions in parens
    where
        p = Proxy @String
```

`expr` can be used to both pretty-print and to parse:

```Haskell
> expr # (Lit 1 `Mul` (Lit 2 `Add` Lit 3)) `Add` Lit 4
"1 * (2 + 3) + 4"

> "1 * (2 + 3) + 4" ^? expr
Just (Add (Mul (Lit 1) (Add (Lit 2) (Lit 3))) (Lit 4))
```

Now, to also get the syntax errors when parsing, we can use the new `^??` operator rather than `lens`'s `^?`:

```Haskell
> "2 + 3 * * 5" ^?? expr
Left "Unexpected \"*\""
```

## Still lacking in the library

* The errors from `syntax-optics` do not currently report source-code locations of syntax errors.
* An `AVerbosePrism` type synonym, similar in spirit to `ALens` and `APrism`, is currently missing. With it the library would not need `RankNTypes` nor `Proxy` parameters which it currently requires (`p` in the example in this post).

I've tried to define `AVerbosePrism` but haven't succeeded so far.
Help and advice from `lens` wizards would be very appreciated!

## Notes

* Title image by [Jean François WITZ](https://commons.wikimedia.org/wiki/File:Reflex_camera_simple_labels.svg).
