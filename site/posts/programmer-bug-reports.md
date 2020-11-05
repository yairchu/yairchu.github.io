---
title: "How programmers should report bugs"
author: Yair Chuchem
date: 2020.09.21
tags: [code, git, teamwork]
description: How programmers should report bugs
image: lego-ninja.webp
---

Programmers can submit better bug reports than normal users or QA engineers can.

First, let's recap the basics which apply to everyone -

## The basic bug report checklist

* Verify that the issue still exists on the latest version
* Search to see if the issue was already reported (avoid duplicates)
* Provide all of the relevant steps to reproduce, with relevant details like software version and system details
* Report additional info like whether the problem is a regression (it used to work in an older version)

## Going futher as a programmer

Programmers can provide additional insight that normal users can't.
The following is phrased for libraries but also applies for internal bug reports -

### Reproduce the problem with the example programs

Try to reproduce the problem with the library's example programs.

Reproducing the problem with the example is useful because everyone has access to it, and it is a better indicator that the bug is actually in the library rather than in user code.

If the bug cannot be reproduced in the example programs, try to see if you can extend them to reproduce it.

### Add a failing test

Can the problem be reproduced nicely in a test? A test to reproduce is can be invaluable to the developer trying to solve it, and having the test in the test suite will help assure that the problem won't resurface in the future!

### Is it a regression? Bisect

Is this problem new? Did it work fine in a previous version? Use `git bisect` to find where it broke! Finding the breaking commit can be invaluable in pinpointing the regression.

You could also leave a comment on the breaking commit and its author may notice. They may have relevant knowledge and be able to help!

### Platform specific issues

Is the problem specific to the kind of hardware that you have? Does it only happen on a specific OS version or distribution? It might be the case that the developers of the library don't have access to a system like yours. So any additional work you can do to offer insight might be invaluable for getting the problem fixed for you!

### Fix the problem

Did you try the previous steps? With any luck, those already provide a lot of insight about the bug, and hopefully even made it obvious. If it's an easy fix, consider adding a PR to your bug report!

### Notes

* Image [by LEGO](https://commons.wikimedia.org/wiki/File:Lloyd.webp), found in wikimedia commons
