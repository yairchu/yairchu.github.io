---
title: "Simplifying Data Types a la Carte using DefaultSignatures"
author: Yair Chuchem
date: Oct 2, 2019
tags: [haskell, ast, dtalc, defaultsignatures]
description: On using DefaultSignatures to simplify DTALC
image: code.jpg
---

[*Data Types a la Carte*](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) (Swierstra, 2008) showed how to construct the following data structure:

```Haskell
data Expr = Val Int | Add Expr Expr
```

From simple and re-usable individual components:

```Haskell
newtype Val e = Val Int deriving Functor
data    Add e = Add e e deriving Functor

type Expr = Fix (Val :+: Add)
```

([`Fix`](http://hackage.haskell.org/package/recursion-schemes/docs/Data-Functor-Foldable.html#t:Fix) is available in the [`recursion-schemes`](http://hackage.haskell.org/package/recursion-schemes) package and [`:+:`](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html#t::-43-:) is available from `GHC.Generics`)

## The Good

This construction allows to write clean and re-usable modular code. For example we can implement evaluation of expressions as so:

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

The beautiful part, which makes this a functional pearl, is that the `Eval` implementations of `Val` and `Add` are usable not just in the `Expr` type defined above, but also in any other expression language which re-uses them, such as:

```Haskell
type Expr2 = Fix (Val :+: Add :+: Mul :+: Pow)
```

## The Ugly

How would we represent an expression, such as `1 + 2` in the type defined above?

The simple way to do it is ```Fix (R1 (Fix (L1 (Val 1)) `Add` Fix (L1 (Val 2))))```.

The usages of `Fix`, `R1` and `L1` are cumbersome, so to make things easier Swiestra showed how to write the expression as ```val 1 `add` val 2``` using an additional type-class and lifting functions per constructor. This makes writing terms convinient, but a few problems remain unsolved:

* For expression types with many possible constructors, we pay a performance penalty for this representation, when compared to a single "flat expression algebra"
* `Expr`'s `Show` is very cumbersome

### Bringing the benefits of Data Types a la Carte to a flat representation

An alternative way to construct `Expr` is:

```Haskell
data Expr a = EVal (Val a) | EAdd (Add a)
    deriving (Generic1, Functor, Eval)
```

Of note here is the derivation of `Eval`, which is enabled by using `DeriveAnyClass` and adding a default implementation in the `Eval` class along with two trivial instances for types from `GHC.Generics`:

```Haskell
class Functor f => Eval f where
    evalAlgebra :: f Int -> Int
    default evalAlgebra :: (Generic1 f, Eval (Rep1 f)) => f Int -> Int
    evalAlgebra = evalAlgebra . from1

instance Eval f => Eval (M1 i c f) where
    evalAlgebra (M1 x) = evalAlgebra x

instance Eval f => Eval (Rec1 f) where
    evalAlgebra (Rec1 x) = evalAlgebra x
```

Now we can write `1 + 2` as ```Fix (EAdd (Fix (EVal (Val 1)) `Add` Fix (EVal (Val 2))))```, which is about as cumbersome as the previous verbose method, but it has several advantages:

* No performance penalty when we have many constructors
* Chains of `L1` and `R1` are replaced with a single constructor with a suitable name, so `Show` is slightly more sensible and we can write a term by hand without checking for the order of the constructors

Note that this method is compatible with Swiestra's classes and combinators and so we could still write the expression as ```val 1 `add` val 2```!

## The Bad

While this approach allowed us to re-use individual components such as `Val` and `Add` in an expression type - Data Types a la Carte style expression types are limited to be simple recursive types. In practice, programming language ASTs tend to consist of multiple mutually-recursive types. For example a language may have top-level declarations which contain types and statements which contain expressions which contain types which contain constraints which contain types and so forth.

I'll expand [on how to extend the Data Types a la Carte approach for more complicated ASTs](https://github.com/lamdu/hypertypes) in a future post.

## The Extensions

For the code snippets above to compile, open your Haskell source file with the following chants:

```Haskell
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, TypeOperators #-}
```

Note that `DefaultSignatures` [was added in GHC 7.2](https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/release-7-2-1.html) at 2011, so at 2008, the Data Types a la Carte paper couldn't use it yet.
