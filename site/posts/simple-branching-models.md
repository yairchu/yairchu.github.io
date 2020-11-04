---
title: "Simple Git branching models (DRAFT)"
author: Yair Chuchem
date: 2020.11.4
tags: [code, git]
description: Two simple Git branching models
image: merging-neutron-stars.jpg
draft: []
---

In this post I'll present two simple software development models, each suitable for different needs:

* The "Model Flow", for very small or single-person teams that make releases occasionally (not often)
* The "Light Flow", for teams releasing updates regularly (weekly/monthly). It is a simplified variant of the well known ["GitFlow"](https://nvie.com/posts/a-successful-git-branching-model/)


**TODO: Better names for the flows?**

## The Modal Flow

![Modal Flow State Machine](/images/modal-flow.svg)

Modality means that the development has cycles that switch between two modes:

* Free dev. At this stage you are more likely to do big changes in the codebase.
* Preparing to ship, aka ["code freeze"](https://en.wikipedia.org/wiki/Freeze_(software_engineering)). At this stage you're only fixing bugs and problems in the product.

If you ever shipped software by yourself, most likely you've implicitly been using this flow, just without calling it by name.

When a team works by this model they have to communicate their current stages and plans of development. If you heard a manager announce "next month we enter a feature freeze!", then you probably used this model.

### Code freeze

A freeze doesn't necessarily mean that all developers should stop working on longer term features.
It only means that bug fixing currently take priority, and that long-term work has to temporarily stay in *feature branches* and not be merged into the `main` branch.

*The mutex analogy: The freeze, and adding features to `main`, are mutually exclusive.*

In this period more extensive testing is done including manual testing and testing of release candidates by beta testers.

Once the release is finalized, the team reverts to the free dev mode and resume normal development for future versions.

### The burden of modality

If you hear an *"Oh, I wasn't aware we're in a feature freeze"*, that's because communication is tricky. Someone may have missed the meeting, and the announcements in the chat channel might have been drowned by other messages.

At this point it might make sense to consider an alternate flow that formalizes the stages in the git branches themselves.

## The Light Flow

![Light Flow Commit Tree](/images/light-flow.svg)

The different types of branches in the diagram:

* The main branch in the middle is colored in yellow
* Feature branches are colored purple, and are surrounded by dashes to signify that they are temporary. Some might be aborted, others will be rebased and will ultimately become normal commits in the main branch
* Release branches are colored in green. When a release is finalized, a `git tag` is used (signified in a rectangular node), and the release branch is then merged back into `main`.

*(the colors are inherited from the diagram for the slightly more complicated [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/))*

### How the flow works

* Developers can freely add new features to `main` at all times!
* A bug tracker is used to keep track of tasks
  * A list of known "blocker" bugs that shouldn't be in a release is maintained
  * A list of new, not yet well tested features in the `main` branch is also maintained
  * For each blocker bug or new feature, the commit which introduced it is identified in its issue
  * When fixing bugs, the commit messages should specify which bugs were fixed
* Preparing a release
  * If the `main` branch happens to be in a good, bug-free state, then we're in luck! A release candidate can be tagged directly on the main branch (like in the simpler ["GitHub Flow"](https://guides.github.com/introduction/flow/))
  * If we still have bugs to fix for the release requires fixing some bugs, then we open a release branch

### Opening a release branch

When the `main` branch is not suitable for release as is, the person in charge of the release will create a branch from a selected commit in the `main` branch, calling it `fixes-VERSION`, or informally "the release branch".

* The bug fixes are added to the release branch
* Once the known bugs appear to be fixed
  * A release candidates is tagged from the release branch
  * If new problems are discovered, fix them and create new release candidates
  * If the candidate is good, ship the release, tag it, and the branch back into `main`. Merged it with a real merge commit rather than a rebase, to keep the real release in the git history!

#### How to choose a branching point for the release branch

The commit messages and the bug tracker help us "taint" the states in the `main` branch with existence of various bugs.

* Some blocker bugs still need to be fixed and are tainting the branch since they were introduced
* Some bugs were already fixed and they are tainting a range of commits

A good point to start the release branch, is one which is relatively untainted, and also includes valueable features and fixes which improve upon the previous release.

If the chosen point doesn't include all the fixes currently available in the main branch, the fixes should be cherry-picked to it.

*TODO: Find or create tooling to assist this process.*

## How does the Light Flow compare to other flows

### Differences from the GitHub Flow

The [GitHub Flow](https://guides.github.com/introduction/flow/) is a very lightweight flow. It has no release branches, and the `main` branch is deployed to production as is (after testing).

Down-side: This requires a very strict standard of quality on the `main` branch that may slow development down. The Light Flow adds release branches which may branch out of the most stable recent state of the `main` branch, and add crucial fixes on top of it. This may add a burden on the developer creating the release, but in return it avoids modality and enables fast-paced development on the `main` branch.

### Differences from GitFlow

*Note that Vincent Driessen, the creator of GitFlow, [currently recommends](https://nvie.com/posts/a-successful-git-branching-model/) the ["GitHub flow"](https://guides.github.com/introduction/flow/) instead.*

* GitFlow calls the main branch `develop`
* GitFlow also has a `master` branch pointing to the latest release
* GitFlow explicitly describes hotfix branches, which branch out of previous releases and add fixes to them. I deliberately chose to omit their explicit description as I believe that they are merely an extension of the release branch, and describing them explicitly might set the wrong examples

The Light Flow's omission of hotfix branches aims to change the focus and incentivize releasing new developments from the `main` branch more often, to avoid accumulating a gap of unreleased and unstable features. Or in three words: faster feedback cycle.

### Does the Light Flow work

I wrote this post to suggest this model for the development of [Pajam](https://pajam.live/). If we choose to adopt this model, after a while I will update this section with our results.

## Notes

* Image credit: [University of Warwick/Mark Garlick](https://en.m.wikipedia.org/wiki/File:Eso1733s_Artist%27s_impression_of_merging_neutron_stars.jpg)
