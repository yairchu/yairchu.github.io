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
* I will also compare them to [the GitHub Flow](https://guides.github.com/introduction/flow/), ["GitFlow"](https://nvie.com/posts/a-successful-git-branching-model/), and ["Trunk Based Development"](https://trunkbaseddevelopment.com/).

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

### How to choose a branching point for the release branch

The commit messages and the bug tracker help us "taint" the states in the `main` branch with existence of various bugs.

* Unfixed bugs taint `main` since they were introduced
* Bugs that were fixed taint only a specific range of commits

A good point to start the release branch is one which is relatively clean, yet also includes valueable features which improve upon the previous release.

If the chosen point doesn't include all the fixes currently available in `main`, we'll `git cherry-pick` them into the release branch.

### Caution with reverts on the release branch

Sometimes we may want to `git revert` a commit only for a release branch.
In this case we should keep in mind that if we merge the release branch as is,
the revert will propagate into `main`.
If we wish to avoid that, we should remember to un-revert the commit!

## How does Light Flow compare to other workflows

### The GitHub Flow

The [GitHub Flow](https://guides.github.com/introduction/flow/) has a `main` branch and feature branches, without release branches.

Its tools to avoid bugs are code reviews for all changes and rolling back faulty versions. If you can un-deploy faulty versions, which is often possible for web apps, and can put in the time and effort to do good code review, then this flow might work well for you.

If you prefer to avoid faulty releases, and prefer to not extensively code review each and every change, then the Light Flow is probably a better fit.

### Comparison to GitFlow

<image alt="GitFlow" src="/images/gitflow.png" width="350px" />

*Note that Vincent Driessen, the creator of GitFlow, [currently recommends](https://nvie.com/posts/a-successful-git-branching-model/) most projects to adopt the GitHub Flow instead.*

The Light flow is a simplified variant of GitFlow. The differences are:

* `main` is called `develop`
* An additional `master` branch points to the latest release
* GitFlow suggests to use explicit merge commits when merging feature branches, while the Light Flow recommends rebases
* GitFlow explicitly describes a process for *hotfix branches*, which branch out of previous releases and add fixes to them. This makes sense for projects which maintain multiple versions. This may happen if new versions of the product are paid upgrades but old version still get bug fixes. For such projects GitFlow is probably a very good choice.

The Light Flow's recommendation for rebasing feature branches and omission of hotfix branches puts an emphasis on integrating new developments faster and releasing them from `main` more often, to avoid accumulating a gap of unreleased and unstable features.

### Comparison to Trunk Based Development

The [Trunk Based Development](https://trunkbaseddevelopment.com/) may seem similar to the light flow, as the difference is small: It prescribes that release branches **should not** be merged back to `main`.

Its site refers to the Light Flow by the name ["Mainline"](https://trunkbaseddevelopment.com/alternative-branching-models/#mainline) (note that their description predates this post), and it considers it as the "diametrically opposite to Trunk-Based Development", and furthermore, recommends not to use it! But personally I'm not convinced, and I'll demonstrate with a simple example how the small difference between workflow affects things:

Imagine that we decided to revert a commit in a release branch.

The following diagram represents the diff between Light Flow and Trunk based in this scenario, in the form of a bright pink cherry-pick commit and pink arrows denoting merges of release branches back to `main`:

![Light Flow vs Trunk Based](/images/light-flow-vs-trunk-based.svg)

Doing `git log release-2.4..release2.5` in this example would list the correct changes list of changes between these releases with the Light Flow, but using Trunk-Based it will be have a misleading result for this log that omits the re-introduction of the reverted change.

## Does the Light Flow work

<a href="https://pajam.live/"><image alt="pajam!" src="/images/pajam-icon.svg" width="75px" /></a>

I wrote this post in order to suggest this model for the development of [Pajam](https://pajam.live/) (btw, if you happen to be a musician that wants to jam with their friends remotely, I highly recommend you to give Pajam a try!).

Update from 2021.01.26: So far this flow is working very well for us!

## Notes

* Discussion on <img src="/images/reddit.svg" alt="reddit" style="width: 20px; display: inline;"/> [r/programming](https://www.reddit.com/r/programming/comments/juja4y/simple_devrelease_workflows/)
* Image credits:
  * Title image (merging neutron stars): [University of Warwick/Mark Garlick](https://en.m.wikipedia.org/wiki/File:Eso1733s_Artist%27s_impression_of_merging_neutron_stars.jpg)
  * GitFlow: [Vincent Driessen](https://nvie.com/posts/a-successful-git-branching-model/)
* The Light Flow Diagram's colors are inspired by the diagram for GitFlow, and were specifically chosen to be consistent with it for easy comparison
* Updates:
  * 2020.11.16: Comparison with "Trunk Based Development"
