---
title: "Nicer Data Types a la Carte with DefaultSignatures"
author: Yair Chuchem
date: Oct 2, 2019
tags: [haskell, ast, dtalc, defaultsignatures]
description: On using DefaultSignatures to improve DTALC
image: ingredients.png
---

Back in 2008, Swierstra's Functional Pearl [*Data Types a la Carte*](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) showed how to construct the following data structure:

```Haskell
data Expr = Val Int | Add Expr Expr
```

from simple and re-usable individual components:

```Haskell
newtype Val e = Val Int deriving Functor
data    Add e = Add e e deriving Functor

type Expr = Fix (Val :+: Add)
```

([`Fix`](http://hackage.haskell.org/package/recursion-schemes/docs/Data-Functor-Foldable.html#t:Fix) is available in the [`recursion-schemes`](http://hackage.haskell.org/package/recursion-schemes) package and [`:+:`](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html#t::-43-:) is available from `GHC.Generics`)

## The Good

This construction allows to write clean and re-usable modular code. For example we can implement evaluation of expressions like this:

```Haskell
class Functor f => Eval f where
    evalAlgebra :: f Int -> Int

eval (Fix expr) = evalAlgebra (fmap eval expr)

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (L1 x) = evalAlgebra x
    evalAlgebra (R1 y) = evalAlgebra y

instance Eval Val where
    evalAlgebra (Val x) = x

instance Eval Add where
    evalAlgebra (Add x y) = x + y
```

The beautiful part, which makes this a functional pearl, is that the `Eval` instances of `Val` and `Add` are usable not just for the `Expr` type defined above, but also for any other expression language which re-uses them, such as:

```Haskell
type Expr2 = Fix (Val :+: Add :+: Mul :+: Pow :+: Etc)
```

## The Ugly

How would we represent an expression, such as `1 + 2` in the type defined above?

The simple way to do it is ```Fix (R1 (Fix (L1 (Val 1)) `Add` Fix (L1 (Val 2))))```.

The usages of `Fix`, `R1` and `L1` are cumbersome, so to make things easier Swiestra showed how to write the expression as ```val 1 `add` val 2``` using an additional type-class and lifting functions per constructor. This makes writing terms convinient, but a few problems remain unsolved:

* For expression types with many possible constructors, we pay a performance penalty for this representation, when compared to a single "flat expression algebra"
* `Expr`'s `Show` is very cumbersome

### Bringing the benefits of Data Types a la Carte to simpler representations

A few years after the paper, in 2011, `DefaultSignatures` [were added in GHC 7.2](https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/release-7-2-1.html). These enable a more direct construction of `Expr`:

```Haskell
data Expr a = EVal (Val a) | EAdd (Add a)
    deriving (Generic1, Functor, Eval)
```

Of note here is the derivation of `Eval` (using `DeriveAnyClass`). Making `Eval` derivable is a simple matter of adding default method implementations in the `Eval` class along with two trivial instances for types from `GHC.Generics`:

```Haskell
class Functor f => Eval f where
    evalAlgebra :: f Int -> Int
    default evalAlgebra :: (Generic1 f, Eval (Rep1 f)) => f Int -> Int
    evalAlgebra = evalAlgebra . from1

deriving newtype instance Eval f => Eval (M1 i c f)
deriving newtype instance Eval f => Eval (Rec1 f)
```

This gives us a few benefits:

* No performance penalty when we have many constructors
* Chains of `L1` and `R1` are replaced with a single constructor with a suitable name, so `Show` is slightly more sensible and we can write a term by hand without checking for the order of the constructors

## The Bad

While Data Types a la Carte allows us to re-use individual components such as `Val` and `Add` in expression types, those are limited to be simple recursive types.

![Haskell's AST](/images/haskell-ast.svg)

In practice, programming language ASTs tend to consist of multiple mutually-recursive types,
and Data Types a la Carte's approach can't help us express those. I'll expand [on how to extend its approach for more complicated ASTs](https://github.com/lamdu/hypertypes) in a future post.

Discussion:

* <img src="/images/reddit.svg" alt="reddit" style="width: 20px; display: inline;"/> [r/haskell](https://www.reddit.com/r/haskell/comments/dcpi4n/nicer_data_types_a_la_carte_with_defaultsignatures/)
