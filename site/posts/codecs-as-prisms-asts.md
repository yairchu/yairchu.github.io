---
title: "Elegant AST Parsing and Building with Prisms"
author: Yair Chuchem
date: 2019.12.29
tags: [programming, declarative-programming, haskell, python, optics, parsing, codecs, construct]
description: Declarative parsing and pretty printing for language ASTs
image: prism-tree.png
---

Following my [previous post](codecs-as-prisms) which suggested the use of `Prism`s for parsing and building,
using a binary format example -
I also want to show how the same idea can work nicely for parsing and builiding programming language syntax.

## Simple AST example

```Haskell
data Expr
    = Lit Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Show, Eq, Generic)
makePrisms ''Expr
```

Here's our `Prism` for parsing and building the above AST:

```Haskell
> expr # Mul (Add (Lit 1) (Lit 2)) (Lit 3)
"(1 + 2) * 3"

expr :: Prism' String Expr
expr =
    tokens .      -- convert string to tokens
    takeExpr .    -- take the expression
    secondOnly [] -- and there should be no remaining tokens

takeExpr :: Prism' [String] (Expr, [String])
takeExpr =
    infixOpLeftRecursion "+" _Add $ -- Additions of
    infixOpLeftRecursion "*" _Mul $ -- multiplications of
    tryMatch (asideFirst _Lit)      -- literals or
        (_Cons . asideFirst _Show) $
    _Cons . firstOnly "(" .         -- expressions in parentheses
        takeExpr . aside (_Cons . firstOnly ")")

```

This uses the following combinators:

* [`_Cons`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Cons.html),
  [`_Show`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Prism.html#v:_Show), and
  [`aside`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Prism.html#v:aside)
  from [`Control.Lens`](http://hackage.haskell.org/package/lens)
* `firstOnly`, `secondOnly`, and `asideFirst` from [the previous post](codecs-as-prisms#parse-build-prism-combinators)
* `tokens`, `infixOpLeftRecursion`, and `tryMatch` are defined in the [appendix](#appendix) at the bottom

## Observations

In the previous post, `Prism`s didn't match up to Python's [Construct](https://construct.readthedocs.io/en/latest/intro.html) in encoding binary protocols, where Construct made good use of structural duck types (though this appears solvable with some effort). However, for programming language syntax `Prism`s seem very elegant imho.

Note how we harness optics' parametricity and composition. In the previous post we parsed `ByteString`s but here we parse `String` and we start by converting them to tokens (ie `[String]`) and parse that.

### Renegade prisms

Unlike the previous post's lawful `Prism`s, this post's parsing is lossy,
so it breaks the [`Traversal` laws](https://artyom.me/lens-over-tea-2#traversal-laws):

```Haskell
> "1 + (2*3)" & expr %~ id
"1 + 2 * 3"
```

If one desires lawful parsing Prisms, their AST representation has to represent white-space and redundant parentheses.

A `Prism` law that is kept is that if you parse what you built you do get it back:

```Haskell
import Test.QuickCheck.Arbitrary.ADT

propParseBack e = (expr # e) ^? expr == Just e

instance Arbitrary Expr where
    arbitrary = genericArbitrary
    shrink = genericShrink

> quickCheck propParseBack
+++ OK, passed 100 tests.
```

### Caveat: meaninful parse errors

When parsing with this `Prism` fails, it offers no useful error-reporting.
But do I believe that this is solvable and I'll address it in future posts.

## Request for feedback

* Do you think that some extra combinators used here (`asideFirst`, `firstOnly`, etc) should belong in [`lens`](http://hackage.haskell.org/package/lens)?
* Or prehaps these combinators should belong in a separate package? How would you call it?
* Any suggestions as for naming these combinators? Other code improvements?
* Image credit: Does anyone know who is the artist for the opening image? (I found it on [the internets](https://www.pinterest.com/pin/800303796254211989/))

Btw: Thanks to Eyal Lotem for reading drafts of this.

## Appendix

### AST parse-build prism combinators

```Haskell
-- Extend a base parsing prism with applications of an operator
infixOpLeftRecursion ::
    Eq a =>
    a ->                        -- The operator's text
    Prism' expr (expr, expr) -> -- The operator constructor's prism
    Prism' [a] (expr, [a]) ->   -- The base parsing prism
    Prism' [a] (expr, [a])
infixOpLeftRecursion operatorText cons sub =
    leftRecursion cons
    (aside (_Cons . firstOnly operatorText . sub) . retuple)
    sub

-- Extend a base parsing prism with extensions to its right side
leftRecursion ::
    Prism' whole cons ->
    Prism' (whole, state) (cons, state) ->
    Prism' state (whole, state) ->
    Prism' state (whole, state)
leftRecursion cons extend base =
    prism' build (fmap parseExtends . (^? base))
    where
        build (x, state) =
            maybe
            (base # (x, state))
            (build . (extend #) . (, state)) (x ^? cons)
        parseExtends x =
            x ^? extend <&> _1 %~ (cons #) & maybe x parseExtends

-- Add an encoding for a sum-type constructor to an existing prism
tryMatch ::
    Prism' whole cons -> -- The sum-type constructor prism
    Prism' src cons ->   -- Parse the constructor contents
    Prism' src whole ->  -- Prism to encode the other options
    Prism' src whole
tryMatch cons parse fallback =
    prism' build (\x -> (x ^? parse <&> (cons #)) <|> x ^? fallback)
    where
        build x = maybe (fallback # x) (parse #) (x ^? cons)

-- Transform a string into tokens
tokens :: Iso' String [String]
tokens =
    iso splitTokens (foldr addToken "")
    where
        addToken x "" = x
        addToken [x] y
            | Char.generalCategory x == Char.OpenPunctuation = x : y
        addToken x (y:ys)
            | Char.generalCategory y == Char.ClosePunctuation = x <> (y:ys)
        addToken x y = x <> " " <> y
        isOp =
            (`elem` [Char.MathSymbol, Char.OtherPunctuation]) .
            Char.generalCategory
        isParen = (`elem` "()[]{}")
        splitTokens "" = []
        splitTokens (x:s:xs) | Char.isSpace s = [x] : splitTokens xs
        splitTokens (s:xs) | Char.isSpace s = splitTokens xs
        splitTokens (x:xs) | isParen x = [x] : splitTokens xs
        splitTokens (x:xs) =
            case splitTokens xs of
            [] -> [[x]]
            ((y:ys) : zs) | not (isParen y) && isOp x == isOp y -> (x:y:ys) : zs
            ys -> [x] : ys
```

* The `retuple` `Iso` was [defined in the previous post](codecs-as-prisms#parse-build-prism-combinators)
* `tryMatch` takes two prisms from the unparsed source and from the resulting structure to the matched pattern. If there were optics for inversed prisms and [partial isomorphisms](https://stackoverflow.com/questions/59426379/optic-for-partial-conversion-on-both-sides/59441415#59441415) then these could be combined into one argument and the existing [`failing`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Combinators.html#v:failing) combinator could replace `tryMatch`.
