---
title: "What is declarative code"
author: Yair Chuchem
date: 2022.08.26
tags: [code, declarative, parsing, ui, optics, automatic-differentiation]
description: Declarative code explained with examples
image: bugs-chasing-man-on-two-skateboards.jpg
---

Some applications' code for saving and loading documents may consist of two separate functions kept in sync: one to translate the document into a stream of bytes and another for the inverse translation. Both mirror the document's format from different perspectives.

Suppose that you made a bug in one of these functions, or between other tasks you simply forgot to maintain it, such that the app's new feature may be saved but isn't loaded. We can hope that the QA team or the test suite would catch it.

This process is reminiscent to trying to ride two skateboards at the same time. Sometimes you would fall, go back to the skateboard left behind and try to do it again. Nailing it is an impressive feat! But when you are being chased by bugs and deadlines, wouldn't it have been easier if the boards were fused together?

<img src="/images/surfing-on-two-surfboards.jpg" alt="Surfing on two surfboards" style="width: 75%"/>

Tools like [Protocol Buffers](https://en.wikipedia.org/wiki/Protocol_Buffers) provide a saner solution. You'll only need to declare your data structure once, and it automatically implements both saving and loading. As a bonus, when using statically typed languages, it also creates the document's data types for you.

### Command line arguments

<sub>(another example, skip <a href="#declarative-code">to next session</a> if you got the point)</sub>

Imagine getting an issue about a command line flag not working as described in the `--help` text, when you know that it's actually a deprecated option which was already removed! But apparently not from the help text..

This wouldn't happen when using libraries like [`argparse`](https://docs.python.org/3/library/argparse.html). When using it you provide a description of the command line arguments and get their parsing (with helpful error messages), and the help text both generated from it, always in sync!

Maintaining the help text separately is somewhat like hanging on to a unicycle when already sporting a jetpack. I think that we can leave the unicycle behind :)

<img src="/images/jetpack-unicycle-girl.jpg" alt="Girl with a jetpack riding a unicycle" style="width: 75%"/>

## Declarative code

The use of Protocol Buffers or `argparse` as descrbied above is called *Declarative Code*.
It means code the doesn't perform a computation of a single task (like saving a document), but rather describes an aspect's essence from which several different processes can be derived.

Here are a few additional examples:

* **Graphical User Interfaces**: Back in the day we would write code to map the document to a UI objects hierarchy, and write additional code to maintain this object hierarchy when the document changes. Libraries like [React](https://reactjs.org) provided a solution where the mapping function alone would suffice, and UI elements would be automatically added, updated, or removed as necessary.
* **Automatic differentitation and Machine Learning**: Computing [gradients](https://en.wikipedia.org/wiki/Gradient) of functions is a very common task in machine learning and in optimization algorithms like [gradient descent](https://en.wikipedia.org/wiki/Gradient_descent). Traditionally the function's implementation would be repeated three times with different variations: the plain function, and the so-called forward and backward phases for the [back-propagation](https://en.wikipedia.org/wiki/Backpropagation) process which computes the gradients. Beside all the work it took, trying to keep those implementations perfectly consistent was very bug prone.
DSLs like [TensorFlow](https://www.tensorflow.org) allow describing the function once to get all of these tasks from it.
* **Data structure manipulation**: ["Optics"](https://www.optics.dev/Monocle/) reify queries and updates of data structures in a modular way. For example the basic "lens" describes a path to a specific field in a structure, and is composable with other lenses to form deeper paths which you can then both read or update. Immutable data structures became trendy in part because optics made them ergonomic.
* **General programming language features**: It can be argued that PL features like [garbage collection](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)) make code more declarative. In retro languages like C programmers manually coded the allocation and disposal of memory address space for data structures. GC allows us to only describe the allocation and get the disposal for free. This provides more than just developer time savings! It eliminates potential memory leaks and corruption bugs. Similarly, type-inference and dynamic typing both save us from the task of describing the typing aspects of our code to the compiler.

<img src="/images/lens-shedding-light.jpg" alt="Lens shedding light" style="width: 75%"/>

## The pros and cons of declarative code

What are the benefits and disadvantages of declarative code?

Pros:

* It is **less error prone** because there is a **single source of truth** rather than several variations that need to be kept consistent. No amount of testing could replace the value this brings in **eliminating bugs**
* The code is more **succint** and **elegant**, as it describes a more abstract **essence** of our program

Cons:

* **Require additional learning**. For example if you want mutually exclusive command line options, instead of adding a straightforward if-statement you would need to read the library's documentation to find how to specify this
* The additional **indirections** often require additional digging when debugging
* **Glass ceilings**: What if what you need to do isn't supported by the libraries? You would have to extend library code, which is a more difficult task
* **Performance**: In some cases, like garbage collection, dynamic typing, or taped-based [automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation#Reverse_accumulation), we do pay a significant performance price for the work saved from us, and hand-crafted undeclarative low-level code may win over the declarative alternative in the benchmarks

In addition to these, declarative code has an additional, perhaps more important advantage, at least for me: *I just like it!*

<img src="/images/bugs-chasing-man-on-one-skateboard.jpg" alt="Riding a single skateboard is easier" style="width: 75%"/>

## Notes

The images for this post were all painted by the extremely talented [DALL-E 2](https://openai.com/dall-e-2/) per my requests. Here are some details on the process and links to their prompts:

* [Steampunk dude riding two skateboards simultaneously](https://labs.openai.com/s/jbYY45SKTyHEGSzzp0ic5JUi) + [chased by a giant bug](https://labs.openai.com/s/m8OGehagBoCtsOlu5KKHeguH) (digital art)
* [Surfer surfing simultaneously on two surfboards, standing with one leg on each. Digital Art](https://labs.openai.com/s/bmW7IXriyoDOxkeZMA0nSveV)
* [Riding a single skateboard](https://labs.openai.com/s/PEUytwoXLE2qux5ZUcuQvPNF) is a variation of an edit of the previous
* [Girl with jetpack riding a unicycle](https://labs.openai.com/s/M2xAxyJZXE9uWKXEQr6ay01P). This one didn't work well as a prompt to DALL-E for me. It was an effort consisting of many attempts, edits and variations until I succeeded getting it right. Unfortunately I didn't save the intermediate steps of the process
* [Skies with clouds of bad code illuminated from the ground by a spotlight with a lambda in its center, digital art](https://labs.openai.com/s/i2gW5ODiWJj1KYojaSYvSx55)

I wish to thank OpenAI for creating this delightful artist!
