---
title: "Poor man's git submodules at the rich tech giant (DRAFT)"
author: Yair Chuchem
date: 2020.11.16
tags: [code, folklore, git, management]
description: A story of a custom SCM system at a tech giant
image: 640px-Winchester_Mystery_House_(door_to_nowhere).jpg
draft: []
---

Nowadays git seems universally adopted, but it wasn't always the case.
In days of lore I have used SourceSafe, Subversion, Perforce, Bazaar, as well as an internal tool.

This post will tell a true story of such an internal tool,
used by a tech giant that originally started their workflow using a monorepo with Perforce.
It was a huge monorepo, so large that they even implemented an internal Perforce clone that could handle its scale. But at some point they found that the plain monorepo workflow wasn't working so well for them.

## The problems in monorepo land

Changes in internal libraries were constantly breaking the projects that used them.

Even though this company had lots of good tests for their projects, the library teams didn't run all of the tests of their users before each change, because that was just *a lot* of tests!

Had they used a repo per project, with submodules for libraries, the users would had been able fix their code according the library changes at their own pace, without anything breaking.
But this company didn't want to lose the monorepo feel, so they looked for different solutions to this problem.

## Genius to the rescue

The person who designed the new mechanism was a genius with a famous algorithm in his namesake. And I'm not talking about any normal algorithm. Djikstra's Algorithm, Huffman Coding, Gaussian Elimination, Knuth-Morris-Pratt, etc, could all make for quite plausible "You Could Had Invented X" posts. One can try to retrace the steps of these clever folks and figure out how they came to invent these algorithms using pretty sensible insights. But for some algorithms, maybe others can but at least I cannot fathom how one could come up with them. This is one of those algorithms. The person was a certifiable genius!

## Modules mechanism

The new solution to the monorepo problems was:

* The repo was split into modules. Even though it still looked like a single repo, modules were defined to own certain folders, and a commit could not cross module boundaries - it can only modify files inside a single module. Widely used internal libraries were turned into modules.
* Normal users of the libraries would not see the latest branch of the modules, but rather they would see the latest release (kind of a like seeing `master` rather than `develop`)
* Releases were made daily. After very extensive testing, at a specific time each day, dev-ops would approve new releases of the core modules.
  * Note that the new releases often had be made with many tests **failing**! As under the modules system, changes to the libraries could no longer be accompanied with fixes to the projects using them that were in different modules!

## Poor man's git submodules

At the end of the internal tech talk introducing the new solution, there was time for questions.
A person in charge of an internal I18N library asked about the following scenario which he often faced:

Occasionally he discovered that some locale should show numbers/percentages/prices differently than previously assumed, and that his I18N library had to be fixed.
Such changes used to be accompanied with fixes to many tests of the library's users,
because those tests tended to verify the previously assumed-correct behaviour.
As this person's library was going to be part of the core libraries module, he was concerned that he could no longer make fixes to the library along with fixes to its users' tests.

The answer was that indeed he couldn't. He could first fix the tests to expect the newly discovered correct behaviour, thus making the tests fail, then fix the library, and wait for the module changes propagate for the tests to pass again. If he didn't want to make his users' tests temporarily fail, or alternatively he could remove the wrong tests, and after the module was released he could add them back.
This library maintainer didn't seem too satisfied with this answer.

Personally, I hadn't had much experience with my project breaking due to library changes with the old mechanism, but after the new modules mechanism was deployed, I did start experiencing these problems more often, the very same problems that the mechanism purportedly solved.

## Not-invented-here syndrom

I believe that multi-repos and `git submodule`s or an equivalent are simply the right solution here.

But this company had a sort of hubris, believing that it does everything better, and that existing solutions are just not suitable for its scale, especially if those solutions had no academic papers to back them up.

> There are three classes of intellects: one which comprehends by itself; another which appreciates what others comprehend; and a third which neither comprehends by itself nor by the showing of others; the first is the most excellent, the second is good, and the third is useless - [Nicollo Machiavelli, 1513 AD](https://en.wikiquote.org/wiki/Niccol%C3%B2_Machiavelli)

Machiavelli described three classes of intellect: innovation, evaluation, and assumption. A good manager needs to evaluate the innovative recommendations of his advisors, both for protection from the [principal-agent problem](https://en.wikipedia.org/wiki/Principal%E2%80%93agent_problem) and just to catch honest mistakes.

In this case, it seems to me like the people calling the shots didn't properly evaluate the solution, and just assumed that the idea coming from a certified genius is scripture which is not to be doubted. But perhaps even geniuses can make mistakes?

## Caveats

I might be wrong on this, and perhaps the modules mechanism has worked better than I personally witnessed or remember. Maybe it just took a while until people got the hang of how to work with the new system.

If you happen to know how it went from there, feel free to tell me and I'll gladly update this post!

## Take aways

* For teams with multiple projects, prefer multiple repos and submodules over monorepos
* Beware of hubris
* Try to evaluate ideas based on their merits even when they come from credentialed experts

## Notes

* [Title image by Spiel](https://commons.wikimedia.org/wiki/File:Winchester_Mystery_House_(door_to_nowhere).jpg)
* The style of this post is inspired by [Rachel by the Bay](https://rachelbythebay.com/w/), which shares a lot of insightful "war stories" from various tech giants
