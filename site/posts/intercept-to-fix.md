---
title: "Intercepting system calls to fix broken software"
author: Yair Chuchem
date: 2020.12.10
tags: [code, c, macos, low-level]
description: Using DYLD_INSERT_LIBRARIES to intercept system calls in macOS
image: vaccine-3314164_640.png
---

Apple sure like to change things, so when my new computer shipped with the new macOS 11.0, some things didn't work - specifically the Haskell compiler, GHC, failed linking my programs with OpenGL and other system libraries.

[The problem](https://gitlab.haskell.org/ghc/ghc/-/issues/18446) is already fixed in the GHC git repository, and I could try building it, but that might send me on new adventures due to more new-version behaviours, so instead I looked into working around the problem by making macOS 11 behave like macOS 10 did in the way that GHC expects!

### Short problem description

When linking with OpenGL, GHC verifies that the file `/System/Library/Frameworks/OpenGL.framework/OpenGL` exists, but it no longer does!

We can't add the file there (not even with `sudo`) because macOS's `/System` folder is special.

### Solution

We can trick GHC to believe that the file exist, and then everything would work!

This can be done by hijacking its calls to the [`stat`](https://en.wikipedia.org/wiki/Stat_(system_call)) system call and returning fake results.

MacOS lets us inject additional code into programs using the `DYLD_INSERT_LIBRARIES` environment variable, and it also supports special pragmas to tell it to replace library functions (aka "interpose" or "hook").

```C
int my_stat (const char* restrict path, struct stat* restrict buf)
{
    if (STARTS_WITH ("/System/Library/Frameworks/", path))
    {
        // Pretend that the file exists
        return 0;
    }
    return stat (path, buf);
}

DYLD_INTERPOSE (my_stat, stat)
```

The above injected code tricks GHC to believe that any file inside `/System/Library/Frameworks/` exists, and that makes it work!

To work around the problem when executing `ghc` from a build system, it takes a bit more work to make sure that the injection propagates to it, but my complete solution isn't too long, see: [github.com/yairchu/macos11-haskell-workaround](https://github.com/yairchu/macos11-haskell-workaround/)

* <img src="/images/reddit.svg" alt="reddit" style="width: 20px; display: inline;"/> [r/haskell discussion](https://www.reddit.com/r/haskell/comments/k9r2cy/workaround_for_haskell_woes_on_macos_11_big_sur/) on this work-around
* I want to get this workaround into the Haskell build tool `stack`, if you want that too then please share your opinion on [the issue](https://github.com/commercialhaskell/stack/issues/5456)!
* FYI: The Linux equivalent of `DYLD_INSERT_LIBRARIES` is called [`LD_PRELOAD`](https://tbrindus.ca/correct-ld-preload-hooking-libc/), and it can do similar things on Linux.
* Image by [LillyCantible](https://pixabay.com/illustrations/vaccine-syringe-antidote-cure-3314164/) from PixaBay.
