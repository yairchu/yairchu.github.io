---
title: "Web tech for desktop UI. Why?"
author: Yair Chuchem
date: 2020.08.30
tags: [code, gui, web]
description: Is using web technology for desktop UIs something we should want
image: umbrella.jpg
draft: []
---

Over the years I've had my fair share of GUI development, using Qt, GTK, JUCE, FLTK, Kivy, GLFW, and web technologies.

By far the most difficult and frustrating of the bunch, with the worst documentation and most confusing APIs, is the web, where people struggle just to [center elements](https://stackoverflow.com/questions/19461521/how-to-center-an-element-horizontally-and-vertically) in an area.

Electron, React Native, [et](https://forum.juce.com/t/introducing-blueprint-build-native-juce-interfaces-with-react-js/34174/2) [cetera](https://ultralig.ht/), enable implementing desktop/mobile UIs using web technologies. Despite my dislike for HTML/CSS, I see tremendous value in those - when one needs to implement a web version anyway, these solutions enable using one implementation for all platforms.

But occasionally I hear people excited about the possibility of using these for desktop-only software. I can't understand why. Maybe folks which started their UI programming journey in the hazing experience which the web is, assume that it's as difficult in other platforms, so they are relunctant to needlessly go through such a process again to learn a new platform. They are relieved by the promise of using what they already know on more platforms. I however, am somewhat horrified.

* Image: "Umbrella with holes, so it won't fly in the wind.". From [@mutzrayom](https://www.instagram.com/p/BiBUP5hh2m9/), by Gil Amsalem and Ariel Vitkon
