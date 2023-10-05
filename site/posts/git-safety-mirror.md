---
title: "Git safety mirror"
author: Yair Chuchem
date: 2023.10.05
tags: [git, code]
description: A utility to create git mirrors which protect against commits lost due to force pushes
image: safety-mirror.jpg
---

When someone `git push --force` to a public repo which is a dependency of your project, the commit you've used may no longer work. Your projects will fail building and you'll be forced to update to a new version as well as lose the reproducibility of your older versions (which are useful for `git bisect`s). That's why you shouldn't `git push --force` to the main branches of public repos.

But what can you do when your dependencies are maintained by other people who do it anyway?

Here's a new solution to this problem: create a "safety mirror" for the project.

## The anatomy of a safety mirror repository

![Git history for a safety mirror](/images/safety-mirror-commits.svg)

* A safety mirror [starts from the repository of `safety-mirror` tool](https://github.com/yairchu/safety-mirror)
* Then the user sets up the repositories and branches to mirror in `sources.json`
* Subsequent runs of `safety-mirror.py` create quasi-merge commits of the sources into the mirror.
  * They differ from typical merge commits in that the mirrored repository's contents are not actually entering main branch of the safety mirror (which stays just the script and its configuration)
  * The purpose these merge commits serve is simply to keep the history of the mirrored repository attached to the safety mirror's history, so that it never gets lost

To then use the safety mirror as a dependency, simply use it instead of the original repository, with the original repository's commit hashes (which are mirrored). Unlike the original repository, when you update your safety mirror no commits will ever get lost.
