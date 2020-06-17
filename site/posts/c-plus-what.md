---
title: "C++ is the undefined language - DRAFT!"
author: Yair Chuchem
date: 2020.06.16
tags: [code, c++, history]
description: C++ is the least standard language in popular use
image: elves-cplusplus.jpg
draft: []
---

C++ [is standardized by the international standards body ISO](https://en.wikipedia.org/wiki/C%2B%2B), so one might expect it to be similar across different operating systems. Ironically, it is the only popular programming language that doesn't provide a consistent cross platform experience.

## Some examples of C++ incompatability

By no means are these complete lists:

### Compile time

* The global namespace is littered differently across platforms. Can you use an innocent looking name like `Point`?
  * VS often won't handle `std::max` but `std::max<int>` or [`(std::max)` will work](https://stackoverflow.com/a/2789509/40916)
* Which standard library functions are available depend on your compiler version. Use `std::gcd`? Your code won't work on Visual Studio 2015
  * Some things are available under different names, like `std::array` vs `std::tr1::array`
  * While not unique to C++, it does have it much worse because due to a lack of package management, developers are encouraged to use the standard library as much as possible
* Xcode limits usage of C++ features if you wish to support older macOS versions
* Standard C constants like `M_PI` are missing in Visual Studio unless you `#define _USE_MATH_DEFINES` before `#include <math.h>`
  * As a consequence, changing the order of includes often breaks compilation!
* Xcode accepts lambda syntax of `[]{}` while VS requires `[](){}`
* Equivalent attributes have different syntaxes - `__attribute__((aligned(32)))` vs `__declspec(align(32))`
* Fake compilation errors: We only get them in the presence of other errors, and upon fixing the "real" errors they disappear along, and these are unique across different compilers

### Run time

* Run-time crashes in Windows upon using `dynamic_cast` with virtual inheritence while they work fine on macOS

## How did we get there

### The original sin - compatibility for your soul

C++ owes its success to being a superset of C, which made the transition easy for users of the popular systems language of the time.

There are many choices to make when designing a language, and the choices should work together if we want to reach a cohesive and sensible design. By inheriting all of C's choices, C++ handicapped its ability of doing this.

C++ has went so far that even its own name is a reference to [a C++ anti-pattern](https://stackoverflow.com/a/24904/40916).

### Embrace, extend, and extinguish

Microsoft of the 1990s and early 2000s famously used [the EEE strategy](https://en.wikipedia.org/wiki/Embrace,_extend,_and_extinguish) in the browser wars.

They have clearly also used it in the PL/OS wars:

* Make the best IDE for C++ (Visual Studio), gain users, and gradually break compatability with other implementations
* Code developed in the IDE will not work in other OSs, less software for those means users will keep preferring Windows
* Profit!

### Complicated standards are bug prone

We all know that programming is difficult. And standartisation is a form of programming. Relatively simple standards like the shapes and sizes of screws can be implemented successfully, but implementing a complicated standard like C++ without errors is not feasable.

## What's next

C++ is currently the king in some industries (games, audio) because no other popular language addresses their needs. Sadly, I don't see this changing in the near future.

Perhaps one day Rust, Pony, D, Zig, Jai, or something that I haven't heard about will overtake C++'s throne. Let's hope, anything but this!

* Image source: [Le Gregman Show](http://gregmanshow.blogspot.com/2016/12/bd-37-le-travail-des-lutins.html), translated by [Monjipour](https://www.reddit.com/r/ProgrammerHumor/comments/d2qfm9/hang_in_there_little_guy_stolen_translated_source/)
