---
title: "Time Travelling Refactor - Experience Report"
author: Yair Chuchem
date: 2020.09.02
tags: [code, git, rebase, refactoring]
description: Refactoring back in time
image: back-to-future-lego.jpg
---

For existing projects, considering switching libraries can be intimidating.
We may not even be certain that the alternative is worth the switch.

Often we'd first evaluate candidate libraries with smaller example projects. This evaluation takes time, it can save us a bigger time sink that could had happened if we discover that the library is actually not a good fit after already having put a lot of work into integrating it.

There' a middle approach between integrating with a toy and just switching on the actual project. In this approach we choose a very specific toy example for evaluation: an early, simpler version of the real project, which we can obtain from our version control history!

When possible, this has several benefits:

* Our evaluation is with something similar to the real project
* The work we're doing for evaluation could be reused for integration in the real project!

Re-using the work for the real project relies on our ability to [merge effectively](/tag/merge). I call this a "time travelling refactor", because we do the refactor of switching libraries "in the past" and then by merging we apply the effects of past changes to the present.

## Experience report

I've recently implemented this approach successfully, performing a library switch that I would otherwise feel intimidated from doing.
I used [`sub-rebase` for splitting the merges to smaller steps](/posts/split-merge-to-smaller-pieces) and [`git-mediate` to resolve the conflicts](/posts/git-mediate-stops-fear).

Did you ever do the "time travelling refactor"? Let me know if you did, how did your experience go, as well as if you know of any existing post on the topic.

* [Image by Jeff Jacowski](https://www.flickr.com/photos/jjackowski/12236722024/)
