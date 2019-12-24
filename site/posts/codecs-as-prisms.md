---
title: "Parsers and Builders as Prisms"
author: Yair Chuchem
date: 2019.12.19
tags: [programming, declarative-programming, haskell, python, optics, parsing, codecs, construct]
description: Declarative parsing and pretty printing
image: Dispersive_prism.png
---

Serialization and deserialization are tedious tasks, often riddled with boiler-plate and bug-prone code. There's a plethora of tools, such as parser combinator libraries, which aim to assist us in some of these tasks, and it seems that new tools never stop popping up. This hints that there are probably no well known satisfactory solutions to these problems, which probably means that most of these tools are ad-hoc rather than principled high-quality solutions.

Are there no good solutions out there? Actually, there are!

Back when I was programming in Python, I have used [Construct](https://construct.readthedocs.io/en/latest/intro.html) to great satisfaction. But for a long time now I have been using Haskell, and have found no equivalent to it, [which I've actually wanted for more than 10 years](https://stackoverflow.com/questions/1225053/haskell-equivalent-of-pythons-construct)! So perhaps I should start looking into making one? Hence this post.

In this post we'll introduce Construct's declarative approach and then discuss how to implement an equivalent principled solution in Haskell, based on optics.

## Construct: declarative parsers and builders

Construct is declarative.

What do I mean by that? A declarative implementation of a parser should describe the format, not how to parse it!

From this description, the computer will automatically figure out how to parse. Not only that, it will also know how to build, and ideally even generate extra artifacts like documentation for the format!

Here's how Construct defines the format of IPv4 packets:

```Python
ipv4 = Struct(
    "header" / BitStruct(
        "version" / Const(4, Nibble),
        "length" / ExprAdapter(Nibble,
            decoder = lambda obj, ctx: obj * 4,
            encoder = lambda obj, ctx: obj // 4,
        ),
    ),
    ...
    "total_length" / Int16ub,
    ...
    "options" / Bytes(this.header.length - 20),
    "body" / Bytes(this.total_length - this.header.length),
)
```

In this simple declaration we created a parser, builder, and the data type for IPv4 packets!

Also, I'd like to highlight the richness of the format. It isn't a simple "struct" with fields of known types, but rather a "dependently-typed" one, where the size of the `options` field depends on the value of `header.length`! Haskell parser combinators libraries which support such dependencies tend to identify as ["monadic parser combinators" libraries](http://hackage.haskell.org/package/parsec), in constrast to ["applicative parser combinators"](http://hackage.haskell.org/package/regex-applicative) which don't support them.

## Parsers and builders as prisms

What is the essence of parsing and building?

```Haskell
data Codec myType encoded = Codec
    { build :: myType -> encoded
    , parse :: encoded -> Maybe myType
    }
```

(Caveat: one may desire meaningful parse errors using `Either ParseError` rather of `Maybe`. We'll ignore this issue for now)

Looking for a principled solution, one may notice that this type is equivalent to [`Prism`](https://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html) from the popular [`lens`](https://hackage.haskell.org/package/lens) package!

> ‘There are only two hard things in Computer Science: cache invalidation and naming things.’ - Phil Karlton

Naming things is hard, and we want principled approaches and code re-use, so we'll choose to use the existing `Prism` rather than make an equivalent new ad-hoc type. Hopefully this will also enable enjoying the fruits of the existing `lens` eco-system.

Let's demonstrate with a simplified IP-like protocol:

* Word8: The constant 7
* Word16: Body length
* Word16: Origin address
* Word16: Destination address
* Body-length bytes: Packet body

```Haskell
> simplifiedIp # ((2, 3), "Hello World!")
"\a\NUL\f\NUL\STX\NUL\ETXHello World!"

> "\a\NUL\f\NUL\STX\NUL\ETXHello World!" ^? simplifiedIp
Just ((2,3),"Hello World!")

simplifiedIp :: Prism' ByteString ((Word16, Word16), ByteString)
simplifiedIp = takeSimplifiedIp . secondOnly ""

takeSimplifiedIp ::
    Prism' ByteString (((Word16, Word16), ByteString), ByteString)
takeSimplifiedIp =
    _Cons . firstOnly 7 . -- Remove the constant byte 7
    takeWord16 .          -- (bodyLen, rest)
    aside (takeWord16 . aside takeWord16) .
                          -- (bodyLen, (origin, (dest, rest)))

    -- Reordering (this is somewhat painful):
    aside retuple .       -- (bodyLen, ((origin, dest), rest))
    retuple .             -- ((bodyLen, (origin, dest)), rest)
    asideFirst swapped .  -- (((origin, dest), bodyLen), rest)
    from retuple .        -- ((origin, dest), (bodyLen, rest))

    aside takeBytes .     -- ((origin, dest), (body, remainder))
    retuple               -- (((origin, dest), body), remainder)
```

This uses some combinators from `Control.Lens` and some extra combinators defined below:

### Parse-build prism combinators

```
firstOnly :: Eq e => e -> Prism' (e, a) a
firstOnly x = asideFirst (only x) . iso snd ((,) ())

secondOnly :: Eq e => e -> Prism' (a, e) a
secondOnly x = swapped . firstOnly x

asideFirst :: APrism s t a b -> Prism (s, e) (t, e) (a, e) (b, e)
asideFirst l = swapped . aside l . swapped

-- Tuple shuffling Iso
retuple ::
    Iso
    (a0, (a1, a2)) (b0, (b1, b2))
    ((a0, a1), a2) ((b0, b1), b2)
retuple =
    iso
    (\(w0, (w1, r)) -> ((w0, w1), r))
    (\((w0, w1), r) -> (w0, (w1, r)))

takeWord16 :: Prism' ByteString (Word16, ByteString)
takeWord16 = _Cons . aside _Cons . retuple . asideFirst (from word16Bytes)

word16Bytes :: Iso' Word16 (Word8, Word8)
word16Bytes =
    iso
    ((both %~ fromIntegral) . (`divMod` 256))
    (\(w1, w0) -> fromIntegral w1 * 256 + fromIntegral w0)

takeBytes ::
    Integral a =>
    Prism' (a, ByteString) (ByteString, ByteString)
takeBytes =
    prism'
    ( \(x, y) ->
        (fromIntegral (ByteString.length x), x <> y))
    (\(count, x) ->
        ByteString.splitAt (fromIntegral count) x <$
        guard (fromIntegral count <= ByteString.length x))
```

## Conclusion

We've succesfully crafted prisms to parse and build our structure!

Now let's review some drawbacks of the presented approach:

* We used anonymous tuples where Construct uses named fields
  * Using tuples isn't so type-safe nor is it descriptive
  * Reordering our tuples to fit the combinators was painful
  * To do what Construct does, we would need structural records, where we can add fields to the structure one at a time (note that this may be possible using a package such as [vinyl](https://hackage.haskell.org/package/vinyl-0.12.0/docs/Data-Vinyl-Tutorial-Overview.html)?)
* Our parsers don't perform error reporting
  * When writing parsers for programming languages this is quite crucial! (Imagine getting a syntax error with the compiler not even pointing out what line it's at)

I believe that both of these problems can be solved, resulting in a powerful and ergonomic principled solution. In my next post I'll describe an approach to add error reporting to our prisms.

Notes:

* [AJ. Kennedy's "Pickler Combinators" paper (JFP 2004)](https://www.microsoft.com/en-us/research/wp-content/uploads/2004/01/picklercombinators.pdf) described parsing+building combinators in Haskell using a custom data type (not using prisms) and demonstrated some more useful combinators for parsing
* Apparently T. Filiba, the author of Construct, [has attempted re-implementing it in Haskell in 2014](http://tomerfiliba.com/blog/ConstructPlusPlus/), but gave up
* Image credit: [Kelvinsong [CC0]](https://commons.wikimedia.org/wiki/File:Dispersive_prism.png)
