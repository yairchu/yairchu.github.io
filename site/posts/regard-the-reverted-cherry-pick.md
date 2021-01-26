---
title: "The dangers of the reverted cherry-pick"
author: Yair Chuchem
date: 2021.01.26
tags: [code, git]
description: Pay attention when merging reverted cherry-picks
image: Back-to-the-future-Fading-family-photo.webp
draft: []
---

Timelines can interact in unexpected ways. Sometimes innocent merges can yield bad results, and different merges which you'd expect to be equivalent can vary dramatically!

This isn't an esoteric problem which only arises in strange edge-cases, it can easily arise in real-world situations! This post will provide a simple example of how this can happen.

![Reverted cherry pick](/images/reverted-cherry.svg)

Suppose that you've worked around a bug in a release branch, and cherry-picked the fix in `main`. Later, after implementing a proper fix for the bug in `main`, you've reverted the work-around.

What would happen if you then merge the release branch into `main`, would the work-around still be reverted?

**No!** The reverted work-around will resurface if you are on `main` and `git merge` the release branch!

(if you are skeptical about this claim, see [example repo](https://github.com/yairchu/git-situations/tree/reverted-cherry-merge) for proof)

![Merge in several steps](/images/reverted-cherry-alt-merge.svg)

On the other hand, if you split your merge to several steps, which is often useful [for conflict resolution ergonomics](/posts/split-merge-to-smaller-pieces), some merge sequences will result with the revert winning (demonstrated [in example repo](https://github.com/yairchu/git-situations/tree/reverted-cherry-alt-merge)).

In such tricky scenarios, you should verify that the results of the merge or rebase look right. In a future post I may announce a tool that will point out which merges are tricky.

## Notes

* The header image is from the film [Back to the Future](https://en.wikipedia.org/wiki/Back_to_the_Future). It depicts how things may disappear when merging timelines carelessly.