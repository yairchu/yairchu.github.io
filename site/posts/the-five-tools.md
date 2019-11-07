---
title: "Why does your programming language have five command line tools?"
author: Yair Chuchem
date: 2019.11.07
tags: [programming, haskell, python, c++, rust, stack]
description: Why do programming languages have so many build tools
image: car-duct-tape.jpg
---

Whichever programming language you use, its eco-system probably consists of a plethora of confusing command line tools which you might have needed to use to actually make software with it.

## Compiling your program

You need something to compile or interpret your program. In Haskell this is `ghc`, in Python it's `python`. In C++ it's ~~`gcc`~~ ~~`clang`~~ your IDE.

## Compiling a program which consists of several modules

In some languages, the compiler only compiles a single source file. If we split our code to several modules, we need a tool to run the compiler for each one. In Haskell that's `cabal` (and later also `ghc --make`), in Python it was always baked in to the interpreter. In C++ this is the reason you just use an IDE.

## Package manager

You want to use libraries. In the old days folks downloaded installers or source packages from the internet and executed their install scripts, but the current trend is to invoke the package manager which magically installs a package along with its transitive dependencies for you.

In Haskell that's `cabal-install`. In Python it was `easy_install` and nowadays it seems to be `pip`. In C++ you simply try to avoid using libraries.

### The package manager fallacy

A problem with this paradigm arises when you work on two projects which depend on different versions of the same library. This is a conflict!

The problem is even more subtle as versions of the transitive dependencies may occasionally matter too.

This is the reason that we need even more tools.

### The "Sandbox"

This tool allows different projects to use distinct versions of libraries. In Haskell this is `cabal-v2`, in Python it was `virtualenv` and nowaday it seems to be `venv`.

### The Libraries Sandbox fallacy

The libraries aren't all of the dependencies. The specific version of the programming language's compiler/interpreter itself also tends to matter quite a lot!

This is the reason that we need even more tools.

## Reproducible Build

![Turtles all the way down](/images/turtles-all-the-way-down.jpg)

You also want to be able to express which exact version of your compiler/etc you are using, and have your tool do all the rest (get this compiler version, your dependencies, and build your program for you).

You want the definitions to be stored in your source repository so that you could easily go and build the old versions of your project just like you did when you implemented them.

In Haskell the tool for this is generally `stack` (which actually existed before `cabal-v2`) or `nix`. In Python folks keep several toolset installations (i.e `python2` and `python3`) and manually invoke the right one. In C++ to build your old code you keep around an old computer.

In a perfect world the "reproducible build" tool should be the only tool you need.

The existence of the previous partial solutions is an awkward artifact of history which represents the order of iterative discovery and partial solutions to this problem space.

### Points for Rust

<img src="/images/dab-emoji.jpg" alt="Dab Emoji" style="width: 150px;"/>

Hindsight is 20/20.

Unlike Python, Haskell, and other languages which each developed their five tools, Rust, a new-comer modern programming language skipped this baggage and does offer a single reproducible build tool to rule them all - `cargo`.

## Is that all

![Yo dawg](/images/yo-dawg-tool-to-get-tools.jpg)

Is that all? Or do we also need a tool to specify the exact version of our reproducible build tool?

In a perfect world you wouldn't need that, but unfortunately the world is not perfect. Haskell's `stack`'s new versions don't support some features of the definition files (`stack.yaml`) of its older versions and vice versa. To solve this problem without an additional layer the reproducible build tool must be solid. It has to be backwards compatible with its older versions and also not have any weird behaviors (a bug-feature) that people rely on. Otherwise we'd need to keep piling layers.

Image sources:

* [Duct-tape car](https://imgur.com/gallery/BwYFH0F)
* [Turtles](https://en.wikipedia.org/wiki/Turtles_all_the_way_down#/media/File:River_terrapin.jpg)
* [Dab Emoji](https://www.change.org/p/apple-add-the-dab-emoji-to-ios)
