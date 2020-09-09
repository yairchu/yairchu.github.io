---
title: "How to organise your git commits"
author: Yair Chuchem
date: 2020.09.03
tags: [code, git]
description: Why and how to organise git commits
image: lego-business-man.jpg
---

Suppose that in a fruitful day of work you:

* Fixed a bug
* Changed some colors and text in the UI
* Added a minor feature
* Renamed some classes

At the end of the day you may either push it all in one commit, or perhaps create a commit for each of those changes. In this post I'll argue that the latter choice is often better.

## The benfits of separate commits

Let's examine how possible future tasks will be affected by keeping these changes in separate commits:

### Reviewing what changed

If you make a PR, or someone wants to review these changes, it would be much easier to do with separate commits. They could quickly skim the rename commit which has lots of repeating boring changes, and they'll see all the bugfix's changes coherently grouped together. For the colors-change commit they may just have a glance on the new look and won't even need to examine its code representation.

### Reverting and cherry-picking

If later you discover that the minor feature is no longer desired, it would be easier to `git revert` it when it's a separate commit. Or if someone wants to grab only the bugfix change to the stable branch, they could easily `git cherry-pick` it when it's a separate commit.

### Bisecting

You may discover some bug in the future, and do a `git bisect` to find what change introduced it. With the commits separate, you'll get better more specific information when it pinpoints which commit introduced it.

### Resolving conflicts

When resolving merge conflicts in [the "small pieces" flow](/posts/split-merge-to-smaller-pieces), a lot less mental effort is involved when the changes are separated and explained in their commit messages.
For example when resolving the conflicts resulting from the rename, sometimes just repeating [the `git-search-replace`](https://github.com/da-x/git-search-replace) followed [by `git-mediate`](/posts/git-mediate-stops-fear) will be enough to resolve them.

### Code archeology

When trying to find where code originated using tools like [DeepGit](https://www.syntevo.com/deepgit/) or [magit](https://magit.vc/), these tools are inteligent enough to recognize file renames and find the actual origin of the code before the rename. But if we mix too many changes together then their heuristics for recognizing renames may fail.

## How to make the commits well organised

Example situation: You made the colors change, then the rename, and then did another color change.

One way to do things is to make a local commit after each change (one should use `git add -p` to make sure they're only adding the relevant changes to each commit). Doing this, after these changes you'll have three commits. Then before pushing, a `git rebase -i` can be used to squash the two color changes into one commit.

## Conclusion

It's good to split commits to coherent units, where renames are separate from bugfixes, etc. It's not only good when working in a team, but even when developing something alone. But note that there's no need to take it to the extreme and split them even further to microscopic commits (then just reviewing the list of commits will become tedious).

## Notes

* This isn't my own original concept, I'm just documenting it. From my experience this practice is moderately, but not universally, wide-spread.
* Why did I write this: I wanted to explain this to a friend but I couldn't find any article in the Internet that I could send, so I wrote this one. Btw if you do know a good article on this topic, please direct me to it!
* Image by <a href="https://pixabay.com/users/CapsandCapital-11428599/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=4250499">Stu G</a> from <a href="https://pixabay.com/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=4250499">Pixabay</a>
