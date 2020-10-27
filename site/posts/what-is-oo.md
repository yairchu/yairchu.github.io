---
title: "What is Object Oriented Programming"
author: Yair Chuchem
date: 2020.10.27
tags: [code, c, c++, oop]
description: What is OOP
draft: []
---

[Wikipedia's definition of OOP](https://en.wikipedia.org/wiki/Object-oriented_programming) is as follows:

> **Object-oriented programming (OOP)** is a programming paradigm based on the concept of "objects" ... programs are designed by making them out of objects ... typically in combination with imperative, procedural programming ... multi-paradigm ...  **TL;DR**

It's a vague and long definition, which as a programmer I'm less attracted to. I prefer descriptions that are useful and succinct.

In this post I'll give a short description of OOP, and demonstrate it by comparing C and C++. Here it is:

## OOP

*Object-oriented programming* extended procedural programming with *namespacing* (especially for *methods*) and *encapsulation*. These features are widespread today, but they were popularized by the OOP movement.

Some additional concepts used to be considered integral to this style, but their importance has dwindled. Notable among them is *inheritence*, which many old texts considered to be the main concept of OOP. Today, *the only distinguishing feature of OOP is method namespacing*.

## Namespacing

Method namespacing allows us to give the same name to different *methods* (aka *functions*).

Consider the following C code:

```C
button_add_listener (submit, this);
/* ^ Note how we manually prefix our function names with type names */
```

It reads much nicer in C++:

```C++
submit.addListener (this);
// ^ Calls the Button::addListener method
```

Another common OO form of namespacing is *overloading*, where the compiler infers which function of the same name to call according to which one best fits the types.

---

Nowadays, there are additional namespacing mechanisms which are universally adopted in all modern programming languages, also in non-object-oriented ones:

Explicit namespaces (`namespace` in C++) provide a general mechanism to disambiguate names. The following Python snippet demonstrates them in the form of module imports:

```Python
import numpy # Qualified import
from numpy import arange # Unqualified import, like "using" in C++.

x = numpy.sin(arange(15))
```

An additional form of namespacing is *interfaces*, or the more general [*typeclasses*](https://en.wikipedia.org/wiki/Type_class). These are not mainly a namespacing feature, but they do allow a form of name reuse in that an interface method call may invoke different methods for different types.

Given the overlapping benefits between the various forms of namespacing, some modern PLs choose not to include the OO concepts of *methods* and *overloading*, and in practice those remains the only distinguishers for whether a language is object-oriented or not.

## Encapsulation

Encapsulation lets us limit member variable access to class methods (ie `private` in C++). It provides several benefits -

### Stable APIs

Users use explicitly defined clean APIs, rather than willy-nilly accessing internals that may be prone to change. This is great!

### Enforcing invariants

Accessing the structure indirectly via class methods allows class implementors to easily impose invariants on the class state.

*Or at least this used to be the case until the rise of concurrency* (aka multi-threading). Nowadays maintaining invariants with mutable state became a challenge involving `std::mutex`es, `std::atomic`es, and migraines.

### Non-OO styles of encapsulation

Procedural languages like C already had some forms of encapsulation with unexposed symbols (ie `static`) and opaque types.

Modern languages support modules with explicit export lists, which provide the same benefits with a more general interface.

## Appendix: Whatabouts

According to Wikipedia there is more to OOP than the two concepts that I mentioned above.

In this section I will attempt to refute this claim.

### Dynamic dispatch

Dynamic dispatch is not uniquely an OO feature. Procedural languages already supported dynamic dispatch via function pointers. Granted, OO did popularize it for a while, but in practice many of the use cases of dynamic dispatch can often be replaced nicely with other mechanisms available in modern PLs such as anonymous functions.

The following OOP style code -

```C++
MyWidget::MyWidget()
{
    ...
    m_submit.addListener (this);
    ...
}

void MyWidget::onClick (Button* button)
{
    if (button == m_submit) { ... }
    else ...
    else abort ("Unexpected button!");
}
```

Is nicer in "modern style":

```C++
MyWidget::MyWidget()
{
    ...
    m_submit.onClick = [=]{ ... };
    ...
}
```

### Design patterns

OOP design patterns are disappearing and being replaced with trivial code using new features, as demonstrated above with the listener pattern is being replaced by using function values. Likewise the visitor pattern was often used to emulate sum-types, and is getting replaced by built-in PL sum-types support.

### Inheritence

Inheritence is a famous OOP concept which isn't commonly used in practice.
As an example, Google's C++ style guide [recommends against using it](https://google.github.io/styleguide/cppguide.html#Inheritance). Given that modern OOP style avoids it, it doesn't belong in a modern description of OOP style.
