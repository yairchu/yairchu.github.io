---
title: The revolution in UI paradigms (draft)
author: Yair Chuchem
date: 2021.09.28
tags: [code, ui, declarative, history]
description: The rise of declarative UI libraries
image: double-bike-336x500.jpg
draft: []
---

For decades we've been developing GUIs using libraries which all worked in a similar ways, until in 2013 Facebook unveiled [React](https://reactjs.org), which has changed the way we write UIs. Its ideas have propagated to other platforms and libraries like Apple's SwiftUI.

![Model and UI code](/images/ui-code.svg)

In this post I'd like to describe what sets apart the modern libraries from traditional ones like Qt, GTK, AppKit, etc, and also trace back earlier origins of the new approach back to 2003 with the "Immediate Mode UI" paradigm ([ZMW](http://perso.univ-lyon1.fr/thierry.excoffier/ZMW/), [Dear Imgui](https://github.com/ocornut/imgui)).

## Elements of a traditional GUI application

The user code of a GUI app traditionally consisted of the following parts:

* Model code (the model is the document that the app is viewing and editing)
  * Data structure definitions of the model
  * Code to save/load the data from files (or from the cloud)
  * Setter methods which modify the document and notify its listeners
  * Listener mechanisms to get notified when parts of the model change
* View/UI code
  * Code to construct the UI elements for the initial document, which would also register event handlers to handle user interactions, and would register listeners to update the UI when the model changes
  * Event handlers reacting to user actions. These would invoke the model's setters to update the document
  * Listener handlers updating the UI when the document changes
  * UI objects destructor which unregister the model listeners

This structure is tricky to get right. The Model and UI stay in sync using listeners that which we may forget to register, and the update handlers need to update the UI in a way that is consistent with how the same state would have been contructed initially, often leaving a scent of code duplication.

## React's approach

React reuses the UI initialization code for UI updates. It does this by comparing the new UI description (aka "virtual DOM") to the previous one, and then adds, removes or updates elements according to the computed diff.

With this approach our model could be a simple data structure, and it no longer needs listener mechanisms or setter methods. The event handlers can just update the data directly. We also don't have to manually write the UI updates code.

While this is a simpler approach with less boiler-plate and repetition,
its down-side is that a complete UI description is computed even when only a small part of the document changes, and this may have a performance cost. Note that SwiftUI and [Svelte](https://svelte.dev) reduce this draw-back using language features that track data dependencies in user code, to only update UI hierarchies whose data sources changed.

## The rationale behind the traditional approach

If React's approach is so simple, why did major companies like Apple, Microsoft, and others make UI libraries that are more difficult to use? Did they just not find the right idea, or did they have good reasons?

The answer is that decades ago, computers were orders of magnitude slower than today, and we needed to program GUIs in the most efficient way possible rather than in ways that are easier for programmers to use.

## Zero Memory Widgets / Imgui

Much earlier than React, in 2003 Thierry Excoffier published the ["Zero Memory Widgets"](http://perso.univ-lyon1.fr/thierry.excoffier/ZMW/) research and GUI library, and in 2005 Casey Muratori published a video lecture on an equivalent approach which he called ["Immediate Mode GUI"](https://caseymuratori.com/blog_0001).

Their approach can be described as "lower level" React. Where in React the UI construction results in a DOM which consists of high-level components like text-boxes and radio buttons, which are still implemented in the browser using the traditional UI approach, Imgui libraries build this approach from the ground up.

As the "zero-memory" term suggests, they do not maintain any in-memory structure of the widgets: The document is the single source of truth! Instead of the library traversing its own structure of widgets, it traverses the document itself using a function provided by the programmer to map the document to the GUI at that moment. The GUI consists of two parts: how it looks, and what code to invoke in response to user events.

How can widgets consume "zero memory", or in other words, have no state? As in a UI there is no more than one "active widget" at a time (a widget where the user's cursor currently is), we only need to maintain the cursor and the editing state for a single widget at a time, so only the active widget does actually have state. Likewise, as one probably wants their application to open a document with the same scrollbar positions and window size as when the user saved it, even these values should be part of the document itself rather than being widgets-only state.

With this approach there is no duplication of structures, making it much simpler and less error-prone than the traditional approach. However, it may consume more CPU due to redrawing the whole window at each frame.

## Disclaimer

I don't have experience with all the UI libraries mentioned above. There just isn't enough time to try them all! I did work with JUCE for about 10 years, FLTK and Qt about 2 years each, a little bit of GTK, AppKit, Kivy, and a also little bit of web-dev. In addition I've been using and developing [Momentu](https://github.com/lamdu/momentu) along with Eyal Lotem for the development of [Lamdu](http://www.lamdu.org).

Due to my partial knowledge, you may very well find innaccuracies or missing key details in this post. Please feel free to send me feedback and corrections and I'll do my best to update it. Despite my knowledge gaps I felt compelled to write this post because I couldn't find any similar overview elsewhere.

### Momentu

Momentu is a declarative/modern GUI library for Haskell with an emphasis of keyboard based editing, animations, and responsive layout features. It will be properly discussed in a future post. Note that Eyal rediscovered the modern approach underlying its design independently in 2011, before React was released, as well as before we have heard about Imgui.

## Notes

* Header image credit: Maybe [monkeyman767](https://www.myconfinedspace.com/2008/11/09/double-bike/)?