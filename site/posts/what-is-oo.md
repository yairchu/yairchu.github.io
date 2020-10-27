---
title: "What is Object Oriented Programming"
author: Yair Chuchem
date: 2020.10.27
tags: [code, c, c++, oop]
description: What is OOP
image: elves-cplusplus.jpg
draft: []
---

[Wikipedia's definition of OOP](https://en.wikipedia.org/wiki/Object-oriented_programming) is as follows:

> **Object-oriented programming (OOP)** is a programming paradigm based on the concept of "objects" ... programs are designed by making them out of objects ... typically in combination with imperative, procedural programming ... multi-paradigm ...  **TL;DR**

It's a vague and long definition, which as a programmer I'm less attracted to. I prefer descriptions that are useful and succinct.

In this post I'll give a short description of OOP, and demonstrate it by comparing C and C++. Here it is:

## OOP

*Object-oriented programming* extended procedural programming with *encapsulation* and *method namespacing*. These now-common features were made popular by the OOP movement.

## Method namespacing

Method namespacing allows us to give the same name to different "methods" (aka "functions").

Tedious C-style code which looked like this:

```C
window_set_size (window, widget_desired_size (widget));
/* ^ Note how we manually prefix our function names with type names */
```

would look much better in C++ style:

```C++
window.set_size (widget.desired_size());
// ^ Implicitly calls the Window::set_size method
```

### Method namespacing vs other namespace mechanisms

Nowadays, programming languages often have other features that already provide a lot of the benefit that method namespacing does:

#### Module/explicit namespaces

Modern languages offer mechanisms to import symbols from modules (`namespace`s in C++) either with or without qualification (`using` in C++).

Demonstrated in Python:

```Python
import numpy # Qualified import
from numpy import arange # Unqualified import, like "using" in C++.

x = numpy.sin(arange(15))
```

#### Interfaces and Typeclasses

Interfaces, and their FP generalization [typeclasses](https://en.wikipedia.org/wiki/Type_class), allow the same "interface method" call to invoke different methods for different instances. This provides a limited form of disambiguation that has overlap with hte value provided by method namespacing.

### Should new languages have method namespacing

Given that module namespacing is in concensus, and my inclination towards typeclasses, if I were to design a PL today I would not add method namespacing to th mix, because the value it adds is diminished by overlap with other features.

## Encapsulation

Encapsulation lets us limit member variable access to class methods (ie `private` in C++).

Encapsulation provides several benefits -

### Stable APIs

Users use explicitly defined clean APIs, rather than willy-nilly accessing internals that may be prone to change. This is great!

### Enforcing invariants

Accessing the structure indirectly via class methods allows class implementors to easily impose invariants on the class state.

*Or at least this used to be the case* until the rise of concurrency (aka multi-threading). Nowadays maintaining invariants with mutable state became a challenge involving `std::mutex`es, `std::atomic`es, and lots of headaches.

### But is encapsulation really an OO feature

Procedural languages like C already had some forms of encapsulation in the form of unexposed symbols (ie `static`) and opaque types.

Modern languages support modules with explicit export lists, which provide the same benefits with a more general interface.

## Appendix: Whatabouts

According to Wikipedia there is more to OOP than the two ideas that I mentioned above.
In this section I will attempt to refute that.

### Dynamic dispatch

Dynamic dispatch is not uniquely an OO feature. Procedural languages already supported dynamic dispatch via function pointers.

Granted, OO did popularize it for a while, but in practice many of the use cases of dynamic dispatch can often be replaced nicely with other mechanisms available in modern PL such as anonymous functions.

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

### Inheritence

Inheritence is a famous OOP concept which isn't commonly used in practice.
As an example, Google's C++ style guide [recommends against using it](https://google.github.io/styleguide/cppguide.html#Inheritance). Given that modern OOP style avoids it, it doesn't belong in a modern description of OOP style.

### Design patterns

OOP design patterns are disappearing and being replaced with trivial code using new features.
The listener pattern is being replaced by using function objects. The visitor pattern was often used to emulate sum-types, and is getting replaced by sum-types.
