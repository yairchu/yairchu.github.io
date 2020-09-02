---
title: "How I organise my commits"
author: Yair Chuchem
date: 2020.09.02
tags: [code, git]
description: Why and how to organise git commits
image: lego-business-man.jpg
draft: []
---

Suppose that in a fruitful day of work I did the following:

* Fixed a bug
* Added a small feature
* Changed some colors and text in the UI
* Renamed some classes

At the end of the day I may either push it all in one commit, or I may create a commit for each of those changes. In this post I'll argue that the latter choice is often better.

## Example Tasks

Let's examine how tasks one might do later are affected by keeping those changes as separate commits:

### Reviewing what changed

If I make a PR, or someone wants to review my changes, it would be much easier with the separate commits. They could quickly skim the rename commit which has lots of repeating boring changes, and they'll see all the bugfix's changes together. For the colors commit they may just have a glance on the new look and skip reviewing its code changes.

### Reverting and cherry-picking

If later I discover that the feature I implemented is no longer desired, it's easier to `revert` when it's a separate commit. If someone wants to take only the fix to the stable branch, it's good that it's a separate commit.

### Bisecting

I may discover some bug in the future, and do a `git bisect`. With the commits separate, I'll get better information when it finds that I introduced the bug when adding the new feature, than if it could only tell me it was in all those changes in a single commit.

### Resolving conflicts

If I know that my conflicting change is a rename, I could more easily use [the `git-mediate` flow](/posts/git-mediate-stops-fear) to resolve it. It takes a lot less thought when the changes are separated and explained in the commit messages.

## How to make the commits well organised

Example situation: I made the colors change commit, then the rename, and then I did another color change.

Assuming that I didn't already push my previous commit, then ideally I'll "rewrite the history" using `git rebase` and put the color changes together in a single commit, because we don't want to go too extreme with splitting our commits.

## Conclusion

It's good to split commits to coherent units, where renames are separate from bugfixes, etc. It's not only good when working in a team, but even when I develop something with myself (I may still later want to `revert` or `bisect`!). But note that there's no need to take it to the extreme and split them even further to microscopic commits (then just reviewing the list of commits will become tedious).

## Notes

* Image by <a href="https://pixabay.com/users/CapsandCapital-11428599/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=4250499">Stu G</a> from <a href="https://pixabay.com/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=4250499">Pixabay</a>
