---
title: The revolution in UI paradigms (draft)
author: Yair Chuchem
date: 2021.08.26
tags: [code, ui, declarative, history]
description: The rise of declarative UI libraries
image: imgui.jpg
draft: []
---

For decades we've been developing GUIs using libraries which all worked in a similar way, until in 2013 Facebook unveiled [React](https://reactjs.org), which has started a revolution in the field. Apple and Google have followed suite and released their own modern UI libraries: SwiftUI and Flutter, which are clearly inspired by React.

In this post I'd like to describe what sets apart the modern libraries from traditional ones (like Qt, GTK, AppKit, Angular, FLTK, Kivy, JUCE, etc), and also trace back the origins of the modern approaches back to 2003 with the "Immediate Mode UI" paradigm ([ZMW](http://perso.univ-lyon1.fr/thierry.excoffier/ZMW/), [Dear Imgui](https://github.com/ocornut/imgui)).

## Traditional GUI paradigm

* A constructor function creates UI widgets (aka views/components) corresponding to your program's model/document, and populates them with data.
* Callbacks/listeners are set up to respond to actions and inputs coming from the UI widgets. These callbacks should propagate the changes made by the user to the document.
* Either the previous callbacks also maintain the UI (create widgets for new items, etc), or additional listeners on the model updates are set up to update the UI accordingly.

In this approach there are two structures (the document and the widgets) that we need to keep in sync. Experience has proved this task to be very challenging and bug-prone.

## Zero Memory Widgets / Imgui

In 2003 Thierry Excoffier published his ["Zero Memory Widgets"](http://perso.univ-lyon1.fr/thierry.excoffier/ZMW/) research and GUI library, and in 2005 Casey Muratori published [a video lecture](https://caseymuratori.com/blog_0001) on this approach which he called "Immediate Mode GUI".

As the "zero-memory" term suggests, in this approach we do not maintain any in-memory structure of widgets which is parallel to the document. The document is the single source of truth. Instead of the library traversing its own structure of widgets, it traverses the document itself using a function provided by the programmer to map the document to a GUI at this moment. The GUI consists of two parts: how it looks, and what code to invoke in response to user events.

How can widgets consume "zero memory", or in other words, have no state? As in a UI there is no more than one "active widget" where the user's cursor currently is, we only need to keep the cursor and editing state for a single widget at a time, so only the active widget has state. Likewise, as one probably wants their application to open a document with the same scrollbar positions and window size as when the user saved it, even these values should be part of the document itself rather than being widgets-only state.

With this approach there is no duplication of structures, making it much simpler and less error-prone. However, it may consume more CPU.

## The rationale for the traditional approach

If Imgui is so simple, why did UI libraries from major companies like Apple, Microsoft, and many others create a significanly more complicated solution?

The answer is that decades ago, computers were orders of magnitude slower than today. Programmers needed to program GUIs in the most efficient way possible rather than the simplest one.

The key benefit of the traditional approach was that it could minimize redrawing. When updating a UI element the widgets' state would maintain which parts would need to be redrawn and only their pixels on the screen were recomputed.

## React's challenge

Suppose you would want to use the simple Imgui approach in a web-based UI. This would mean that you would need to reconstruct the visual appearance of the page on every change or cursor movement, and would not be able to reuse HTML's built-in UI elements which are stateful and work in the traditional way. Also, when doing all this in a relatively slow language like Javascript, your web app wouldn't be the fastest.

React solved this challenge by bridging between the two approaches: The user still creates a simple function producing a UI description (aka "virtual DOM") for a given model, but React then creates the standard widgets (aka DOM) from it. When the UI updates, Reacts compares the UI description to the previous one, and changes the widgets accordingly.

The developers of SwiftUI were in a similar situation: How to re-use the existing AppKit while providing a good experience similar to React. In their case, having more control on designing the programming language and compiler, they had the option to make something a little bit more efficient: The library can tell which widgets depend on which model data and only recompute those.

## Disclaimer

I don't have experience with all the UI libraries mentioned above, there just isn't enough time to try them all. I did work with JUCE for about 10 years, FLTK and Qt about 2 years each, a little bit of GTK, AppKit, Kivy, and a also little bit of web-dev. In addition I've been using and developing [Momentu](https://github.com/lamdu/momentu) along with Eyal Lotem in the development of [Lamdu](http://www.lamdu.org).

Due to my partial knowledge, you may very well find innaccuracies or missing key details in this post. Please feel free to send me feedback and corrections and I'll do my best to update it. Despite my knowledge gaps I felt compelled to write this post because I couldn't find any similar overview elsewhere.

### Momentu

Momentu is a declarative/modern GUI library for Haskell with an emphasis of keyboard based editing, animations, and responsive layout features. It will be properly announced in a future post. Note that Eyal rediscovered the modern approach underlying its design independently in 2011, before React was released, as well as before we have heard about Imgui.
