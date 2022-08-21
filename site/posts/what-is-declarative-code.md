---
title: "What is declarative code"
author: Yair Chuchem
date: 2022.08.21
tags: [code, declarative, optics, parsing]
description: Declarative code explained with an example app.
draft: []
---

*Declarative code* doesn't do one specific thing, but rather describes a structure from which several processes can be derived.

For example, take a look at this app:

**TODO:** A fun app maybe of animations consisting of configurable layers with effects

We will look on four different aspects of this app which can be implemented either declaratively, or not:

* Graphical user interface
* Saving and loading documents
* Internal data manipulations
* Command line options (for the standalone version of this app)

We will conclude with discussing the trade-offs involved with the declarative approach.

## GUIs

Traditional GUI frameworks (like GTK or Qt) require your app to implement the following two pieces of code:

* Upon loading a document, create the UI elements representing it
* When the document changes, maintain the UI, by updating or possibly adding and removing elements from it

A declarative GUI library (like SwiftUI or Imgui) requires the user to implement a single mapping from a document to a UI, and uses it for both the initialization step and for updates.

## Saving and loading

Concerning a data model and how to save it, an app would usually have:

* A function to serialize the document to a file
* A completely separate function to parse a file into a document
* Migration code to load documents saved by older versions
* And additionally, statically typed languages may also require definitions for the document's data types

A library such as Protocol Buffers or Construct allows generating these tasks from a single succinct declaration. When PB's default values mechanism doesn't suffice, migration may require some additional logic, but this extra code is probably truly unavoidable.

## Internal data manipulation

TODO: Describe the use of optics to create both getters and setters/modifiers.

## Command line options

The standalone version of this app (**TODO**: link) has command line options, which control the window size and whether to use dark-mode.

Naively one might write code to parse these options, and separately write documentation for them. A declarative library generates both from a single description.

## Declarative code: Pros and Cons

