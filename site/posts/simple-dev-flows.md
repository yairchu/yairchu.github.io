---
title: "Simple Dev/Release Workflows"
author: Yair Chuchem
date: 2020.11.05
tags: [code, git, teamwork]
description: Two simple development models
image: merging-neutron-stars.jpg
---

In this post I'll introduce very simple software development workflows, each suitable for different needs:

* The "Red Light, Green Light Flow", suitable for occasional releases
* The "Light Flow", suitable for regular releases (weekly/monthly)
* I will also compare them to the [GitHub Flow](https://guides.github.com/introduction/flow/) and to ["GitFlow"](https://nvie.com/posts/a-successful-git-branching-model/).

## The Red Light, Green Light / Feature Freeze Flow

![RLGL State Machine](/images/rlgl-flow.svg)

Like in the famous children's game of "Red Light, Green Light" (aka "Statues", and [many other names](https://en.wikipedia.org/wiki/Statues_(game))), development switches between two modes:

* Free dev. New features are developed freely and big changes may happen to the codebase, often introducing bugs.
* Preparing to ship, aka ["code freeze"](https://en.wikipedia.org/wiki/Freeze_(software_engineering)). At this stage bugs and problems are fixed.

When preparing releases and not deliberately choosing a workflow, work tends to naturally develop into the RLGL flow. When you find that there are too many bugs, it may come naturally to decide: "let's focus on fixing bugs now and keep the new features for later".

*The mutex analogy: A freeze is mutually exclusive with adding features to `main`.*

### Challenges with statefulness and concurrency

If you ever heard an *"Oh! I wasn't aware that we're in a feature freeze"*, that's because communication is tricky. Someone may have missed a meeting, or announcements in the group chat may have been drowned by other messages. If you find coordinating the state to be tricky, it may make sense to use a flow that formalizes the modes of development in the structure of the git branches.

## The Light Flow

![Light Flow Commit Tree](/images/light-flow.svg)

The different types of branches and commits in the diagram:

* The main branch in the middle is colored in yellow
* Feature branches are colored purple. They are surrounded by dashes to signify that they are temporary: some might be aborted, others will be rebased and will ultimately become normal commits in `main`
* Release branches are colored in green. They end up with a release and are merged into `main`
* Releases, given `git tag`s, are displayed as rectangular nodes

### How to Light Flow

* Developers can freely add new features to `main` at all times!
* A bug tracker is used to keep track of tasks
  * A list of known bugs that shouldn't be in a release is maintained
  * A list of fresh and not yet well tested features in the `main` branch is also maintained
  * For each bug or feature, the commit which introduced it is identified in its issue
  * Bugfix commits should mention which bugs were fixed
* Preparing a release
  * If the `main` branch happens to be in a good bug-free state, then we're in luck, and just `git tag` it as a release candidate!
  * If we still have bugs to fix for the release, then we open a release branch

### Using a release branch

When the `main` branch is not suitable for release as is, the person in charge of the release will create a branch from a selected commit in `main`, calling it `fixes-VERSION`, or informally "the current release branch".

* Bug fixes are added to the release branch
* Once the known bugs appear to be fixed, tag and build a release candidate
* If new issues are discovered in the RC, go back to the previous stage
* When the RC is good: ship the release, tag it, and merge the branch back into `main`. Merge it - don't rebase, so that the actual release is in `main`'s history

#### How to choose a branching point for the release branch

The commit messages and the bug tracker help us "taint" the states in the `main` branch with existence of various bugs.

* Unfixed bugs taint `main` since they were introduced
* Bugs that were fixed taint only a specific range of commits

A good point to start the release branch is one which is relatively clean, yet also includes valueable features which improve upon the previous release.

If the chosen point doesn't include all the fixes currently available in `main`, we'll `git cherry-pick` them into the release branch.

## How does Light Flow compare to other workflows

### The GitHub Flow

The [GitHub Flow](https://guides.github.com/introduction/flow/) has a `main` branch and feature branches, without release branches.

Its tools to avoid bugs are code reviews for all changes and rolling back faulty versions. If you can un-deploy faulty versions, which is often possible for web apps, and can put in the time and effort to do good code review, then this flow might work well for you.

If you prefer to avoid faulty releases, and prefer to not extensively code review each and every change, then the Light Flow is probably a better fit.

### Comparison to GitFlow

*Note that Vincent Driessen, the creator of GitFlow, [currently recommends](https://nvie.com/posts/a-successful-git-branching-model/) most projects to adopt the GitHub Flow instead.*

The Light flow is a simplified variant of GitFlow. The differences are:

* `main` is called `develop`
* An additional `master` branch points to the latest release
* GitFlow suggests to use explicit merge commits when merging feature branches, while the Light Flow recommends rebases
* GitFlow explicitly describes a process for *hotfix branches*, which branch out of previous releases and add fixes to them. This makes sense for projects which maintain multiple versions. This may happen if new versions of the product are paid upgrades but old version still get bug fixes. For such projects GitFlow is probably a very good choice.

The Light Flow's recommendation for rebasing feature branches and omission of hotfix branches puts an emphasis on integrating new developments faster and releasing them from `main` more often, to avoid accumulating a gap of unreleased and unstable features.

## Does the Light Flow work

I wrote this post in order to suggest this model for the development of [Pajam](https://pajam.live/) (btw, if you happen to be a musician that wants to jam with their friends remotely, I highly recommend you to give Pajam a try!).

In the future, I will update this section with our results.

## Notes

* Image credit: [University of Warwick/Mark Garlick](https://en.m.wikipedia.org/wiki/File:Eso1733s_Artist%27s_impression_of_merging_neutron_stars.jpg)
* The Light Flow Diagram's colors are inspired by the diagram for [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/), and were specifically chosen to be consistent with it for easy comparison
