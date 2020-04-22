---
title: "Basic error reporting for optics"
author: Yair Chuchem
date: 2020.01.02
tags: [code, haskell, optics]
description: Adding error reportinh to folds, traversals and prisms
image: railway-crossing.jpg
---

When [`^?`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Fold.html#v:-94--63-) returns `Nothing`, it is often desired to know why.

Let's define a `^??` operator which returns an `Either` instead of a `Maybe`:

```Haskell
newtype ConstEither e r a = ConstEither { getConstEither :: Either e r }
    deriving Functor

infixl 8 ^??
(^??) :: s -> LensLike' (ConstEither e a) s a -> Either e a
whole ^?? f = f (ConstEither . Right) whole & getConstEither
```

The standard optics (`Traversal`, `Prism`, etc) do not work with our new combinator, so let's see how we can define ones which would.

[`Traversal s t a b`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Traversal.html#v:Traversal) is `forall f. Applicative f => (a -> f b) -> s -> f t` and it uses `f`'s `pure` in the empty case, so we'll replace the `Applicative` with a verbose variant which supplies error information in the empty case:

```Haskell
class Apply f => VerboseApplicative e f where
    vpure :: e -> a -> f a

type VerboseTraversal e s t a b =
    forall f.
    VerboseApplicative e f =>
    LensLike f s t a b

type VerbosePrism e s t a b =
    forall p f.
    (Choice p, VerboseApplicative e f) =>
    Optic p f s t a b

type VerboseTraversal' e s a = VerboseTraversal e s s a a
type VerbosePrism' e s a = VerbosePrism e s s a a

-- Verbose optics support for (^.) and (^..)
instance Monoid r => VerboseApplicative e (Const r) where
    vpure _ = pure

-- Verbose optics support for `preview`, aka (#)
instance VerboseApplicative e Identity where
    vpure _ = pure

-- Verbose optics support for our (^??)
instance Apply (ConstEither e r) where
    ConstEither x <.> _ = ConstEither x
instance e ~ e' => VerboseApplicative e (ConstEither e' r) where
    vpure e _ = ConstEither (Left e)
```

Now we may want an operator to transform optics into verbose optics:

```Haskell
-- Given an error message constructor, turns:
-- * Traversal to VerboseTraversal
-- * Prism to VerbosePrism
verbose ::
    (Profunctor p, VerboseApplicative e f) =>
    (t -> e) ->
    Optic p (Lift f) s t a b ->
    Optic p f s t a b
verbose e t =
    rmap f . t . rmap Other
    where
        f (Other r) = r
        f (Pure r) = vpure (e r) r

-- A fixed variant of transformers:Control.Applicative.Lift -
-- Turns an Apply to an Applicative
-- (transformer's versions Applicative instance requires Applicative f)
data Lift f a = Pure a | Other (f a)
    deriving Functor

instance Apply f => Applicative (Lift f) where
    pure = Pure
    Pure f <*> Pure x = Pure (f x)
    Pure f <*> Other x = Other (f <$> x)
    Other f <*> Pure x = Other (f <&> ($ x))
    Other f <*> Other x = Other (liftF2 ($) f x)
```

Note that I haven't found how to make `verbose` also turn a [`Fold`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Fold.html#t:Fold) to a verbose variant.

To see our verbose optics in action we'll make some verbose variants of optics from [`lens-aeson`](https://hackage.haskell.org/package/lens-aeson/docs/Data-Aeson-Lens.html):

```Haskell
type Err = String

v_Value :: (AsValue t, Show t) => VerbosePrism' Err t Value
v_Value = verbose (\x -> "Doesn't parse as JSON: " <> show x) _Value

v_Double :: (ToJSON t, AsNumber t) => VerbosePrism' Err t Double
v_Double = verbose (expectJson "number") _Double

vnth :: (ToJSON t, AsValue t) => Int -> VerboseTraversal' Err t Value
vnth i = verbose (expectJson ("item at index " <> show i)) (nth i)

expectJson :: ToJSON a => String -> a -> Err
expectJson e x =
    "Expected " <> e <> " but found " <>
    Data.ByteString.Lazy.Char8.unpack (encode x)
```

Now let's see they work:

```Haskell
# Verbose traversals can work like regular traversals

> "[1, \"x\"]" ^? _Value . nth 0 . _Double
Just 1.0
> "[1, \"x\"]" ^? v_Value . vnth 0 . v_Double
Just 1.0
> "[1, \"x\"]" ^? v_Value . vnth 1 . v_Double
Nothing

# But using ^?? rather than ^? we can also get error info

> "[1, \"x\"]" ^?? v_Value . vnth 0 . v_Double
Right 1.0
> "[1, \"x\"]" ^?? v_Value . vnth 1 . v_Double
Left "Expected number but found \"x\""
> "[1, \"x\"]" ^?? v_Value . vnth 2 . v_Double
Left "Expected item at index 2 but found [1,\"x\"]"
> "hello" ^?? v_Value . v_Double
Left "Doesn't parse as JSON: \"hello\""
```

## Notes

[In the previous post's discussion](https://www.reddit.com/r/haskell/comments/eh4gpg/elegant_ast_parsing_and_building_with_prisms/fcl7dvv/), [`lens`](https://github.com/ekmett/lens)'s creator Edward Kmett noted that in `lens`'s early days they experimented with a different formulation of error-reporting optics that placed the extra information in `Optic p f s t a b`'s `p` rather than `f`, but that with that formulation they ran into problems with inference and that this new formulation may work better.

## Request for feedback

* Do you have use cases for this? If so, do you think that this should belong in `lens`?
* Should it belong in a separate package? Perhaps along with the previous posts' `Prism` combinators and with additional optics like [inverted `Prism`s and partial `Iso`s](https://github.com/ekmett/lens/issues/904)?
* Any code suggestions or improvements?

Discussion

* <img src="/images/reddit.svg" alt="reddit" style="width: 20px; display: inline;"/> [r/haskell](https://www.reddit.com/r/haskell/comments/ej15ar/basic_error_reporting_for_optics/)

[Image source](https://www.peakpx.com/637476/railway-line-train-railway-crossing-sky-no-people)
