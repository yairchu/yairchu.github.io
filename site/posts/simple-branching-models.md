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

* The "Modal Flow", for very small or single-person teams that make releases occasionally (not often)
* The "Light Flow", for teams releasing updates regularly (weekly/monthly). It is a simplified variant of the well known ["GitFlow"](https://nvie.com/posts/a-successful-git-branching-model/)


**TODO: Better names for the flows?**

## The Modal Flow

![Modal Flow State Machine](/images/modal-flow.svg)

Modality means that the development has cycles that switch between two modes:

* Free dev. At this stage you are more likely to do big changes in the codebase.
* Preparing to ship, aka ["code freeze"](https://en.wikipedia.org/wiki/Freeze_(software_engineering)). At this stage you're only fixing bugs and problems in the product.

Even when not specifically planning to use this flow, many teams may find themselves using it. It tends to happen when you need to cut a release and find that there are two many bugs in the main branch. In that case it tends to come naturally to decide: "let's focus on fixing these bugs now and keep the new features for later".

*The mutex analogy: A freeze is mutually exclusive with adding features to `main`.*

### The burden of modality

If you ever hear an *"Oh, I wasn't aware we're in a feature freeze"*, that's because communication is tricky. Someone may have missed a meeting, or announcements in the chat channel may have been drowned by other messages. If you find coordinating the state to be tricky it may make sense to use a flow that formalizes the modes of development in the structure of the git branches.

## The Light Flow

![Light Flow Commit Tree](/images/light-flow.svg)

The different types of branches and commits in the diagram:

* The main branch in the middle is colored in yellow
* Feature branches are colored purple. They are surrounded by dashes to signify that they are temporary: some might be aborted, others will be rebased and will ultimately become normal commits in `main`
* Release branches are colored in green. They end up with a release and are merged into `main`
* Releases are given `git tag`s, and are displayed as rectangular nodes

*(the colors are inherited from the diagram for [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/))*

### How the flow works

* Developers can freely add new features to `main` at all times!
* A bug tracker is used to keep track of tasks
  * A list of known "blocker" bugs that shouldn't be in a release is maintained
  * A list of new and not yet well tested features in the `main` branch is also maintained
  * For each blocker bug or new feature, the commit which introduced it is identified in its issue
  * When fixing bugs, the commit messages should specify which bugs were fixed
* Preparing a release
  * If the `main` branch happens to be in a good, bug-free state, then we're in luck, and we just `git tag` it as a release candidate!
  * If we still have bugs to fix for the release, then we open a release branch

### Opening a release branch

When the `main` branch is not suitable for release as is, the person in charge of the release will create a branch from a selected commit in the `main` branch, calling it `fixes-VERSION`, or informally "the release branch".

* Bug fixes are added to the release branch
* Once the known bugs appear to be fixed
  * A release candidates is tagged from the release branch
  * If new problems are discovered, we keep fixing them and later create a new release candidate and repeat
  * When the candidate is good: ship the release, tag it, and merge the branch back into `main`. Merge it with a real merge commit rather than rebasing, to keep the actual release in `main`'s history

#### How to choose a branching point for the release branch

The commit messages and the bug tracker help us "taint" the states in the `main` branch with existence of various bugs.

* Unfixed bugs taint `main` since they were introduced
* Bugs that were fixed taint only a specific range of commits

A good point to start the release branch is one which is relatively clean, yet also includes valueable features which improve upon the previous release.

If the chosen point doesn't include all the fixes currently available in `main`, we'll `git cherry-pick` these fixes into the release branch.

## How does the Light Flow compare to other flows

### Differences from the GitHub Flow

The [GitHub Flow](https://guides.github.com/introduction/flow/) is very lightweight. It has no release branches, and the `main` branch is deployed to production as is (after testing).

I believe that to really make it work, one has to, either explicitly or implicitly, impose feature freezes, and this means that for projects where stability is a priority, the GitHub Flow tends to actually be the Modal Flow describes above.

### Differences from GitFlow

*Note that Vincent Driessen, the creator of GitFlow, [currently recommends](https://nvie.com/posts/a-successful-git-branching-model/) most projects to adopt the ["GitHub flow"](https://guides.github.com/introduction/flow/) instead.*

* GitFlow calls the main branch `develop`
* GitFlow additionally has a `master` branch pointing to the latest release
* GitFlow explicitly describes a process for *hotfix branches*, which branch out of previous releases and add fixes to them. This makes sense for projects which maintain multiple versions, and for such projects GitFlow is probably a good choice.

The Light Flow's omission of hotfix branches aims to put an emphasis on releasing new developments from `main` more often, to avoid accumulating a gap of unreleased and unstable features.

### Does the Light Flow work

I wrote this post in order to suggest this model for the development of [Pajam](https://pajam.live/) (btw, if you happen to be a musician that wants to jam with their friends remotely, I highly recommend you to give Pajam a try!)

If we indeed decide to adopt this model, then after a while I will update this section with our results.

## Notes

* Image credit: [University of Warwick/Mark Garlick](https://en.m.wikipedia.org/wiki/File:Eso1733s_Artist%27s_impression_of_merging_neutron_stars.jpg)
