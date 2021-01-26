---
title: "The reverted-cherry mergency"
author: Yair Chuchem
date: 2021.01.26
tags: [code, git, merge]
description: Pay attention when merging reverted cherry-picks
image: Back-to-the-future-Fading-family-photo.webp
---

Timelines can interact in unexpected ways. When seemingly innocent merges yield unexpected results, we call these situations *"mergencies"* (merge-emergency).

Mergencies can easily arise in real-world situations! They are by no means limited to esoteric edge-cases. If you use git, you better be aware of this issue.

This post will demonstrate the problem with a simple real-world example.

![Reverted cherry pick](/images/reverted-cherry.svg)

Suppose that you've worked around a bug in a release branch, and cherry-picked the work-around in `main`. Later, after implementing a proper fix for the bug in `main`, you've reverted the work-around, which you no longer want.

Assuming that the work-around and fix do not conflict, what would happen if you would merge the release branch into `main`? Would the work-around still be reverted?

**No!** The reverted work-around will resurface if you are on `main` and `git merge` the release branch! (if you are skeptical about this claim, see [example repo](https://github.com/yairchu/git-situations/tree/reverted-cherry-merge) for proof)

![Merge in several steps](/images/reverted-cherry-alt-merge.svg)

On the other hand, if you split your merge to several steps, which is often useful [for conflict resolution ergonomics](/posts/split-merge-to-smaller-pieces), some merge sequences will result with the revert succeeding (demonstrated [in example repo](https://github.com/yairchu/git-situations/tree/reverted-cherry-alt-merge)).

In such tricky scenarios, at least anything involving reverts, you should verify that the results of the merge or rebase look right.

## This situation is real

<a href="https://pajam.live/"><image alt="pajam!" src="/images/pajam-icon.svg" width="75px" /></a>

This post was inspired by a real situation encountered during the development of [Pajam](https://pajam.live/) (btw, if you want to jam musically with your remote friends, give Pajam a try, [it's fun!](https://youtu.be/F9hMTuaH17o?t=2873))

## Can tooling solve the problem?

### A completely different model: Darcs/Pijul

Theoretically, using [Darcs](http://darcs.net) or [Pijul](https://pijul.org), alternative version-control systems with radically different branching models would alledgely [solve this problem correctly](https://tahoe-lafs.org/~zooko/badmerge/simple.html). But contenders don't have the wide adoption nor probably the maturity of git, so I hadn't had the chance to try them out, so I cannot speak about them from experience.

### git-imerge

[`git-imerge`](https://github.com/mhagger/git-imerge) does successfully yield the correct merge result in our example.
But unfortunately [in other cases](https://github.com/yairchu/git-situations/commit/d35f7dc0872ee47b3a16dbe6d09a9fab84c71db3) it yields incorrect merge results!

### A new tool?

I'm considering creating a tool for safer merging. My general idea is that when an intermediate merge would change the merge result, it means that there's a manual decision/resolution required. A large obstacle for creating this tool is picking a name.

If such a tool already exists, please let me know!

## Notes

* I learned the term "mergency" from [Eyal Lotem](https://twitter.com/EyalL)
* The header image is from the film [Back to the Future](https://en.wikipedia.org/wiki/Back_to_the_Future). It depicts how things may disappear when merging timelines carelessly.