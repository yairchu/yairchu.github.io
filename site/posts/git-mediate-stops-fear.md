---
title: "How git-mediate made me stop fearing merge conflicts!"
author: Yair Chuchem
date: 2016.12.28
tags: [programming, source-control, git, merge-conflicts]
description: Quick intro to git-mediate
image: mine-sweeper-flow.png
---

If you’ve ever had to deal with git merge conflicts, then there’s a good chance that these messages give you the chills:

    Auto-merging <SOURCE-FILE-PATH>
    CONFLICT (content): Merge conflict in <SOURCE-FILE-PATH>
    Automatic merge failed; fix conflicts and then commit the result.

They arise when performing merges, reverts, and cherry-picks, and they scare people away and make them reconsider their actions.

![git mergetool --tool=kdiff3](/images/kdiff3.png)

Solving these conflicts [was hard](http://stackoverflow.com/questions/161813/how-to-resolve-merge-conflicts-in-git),
time consuming and very error-prone,
using either fancy 3-way GUI merge tools or doing it manually (did I mention error prone?).

![When resolving conflicts we have to be careful, do it correctly, and avoid introducing bugs](/images/mine-sweeper-losing.jpeg)

Then I learned about [git-mediate](https://github.com/Peaker/git-mediate), a small open-source tool by Eyal Lotem, which helps resolve merge conflicts rather easily, and most important, **correctly**!

How does it work?

First, configure git to use its “diff3” merge conflict style:

    git config --global merge.conflictstyle diff3

Now when conflicts appear, they look like this in the affected files:

![An unresolved git merge conflict](/images/diff3-style.jpeg)

When a conflict occurs, look at it and try to figure out what changed between the base and either the upper or lower branch — choose whichever looks like a simpler change. Now apply this change on the two other parts that don’t yet have it: both the base and the other branch. After your edits, the conflict *should* look like this:

![Mouth added to both HEAD and base chunks](/images/diff3-resolved.jpeg)

Now we run git-mediate which rewrites the file with one where the conflict is resolved and `git add`s it if no conflicts remain.

How does it work? It notices that for this conflict one of the branches matches the base, and replaces the conflict with the *other* branch, which now contains **both changes**.

If git-mediate finds that, after our changes, neither branch is identical to the base, it means that we missed parts of the change. That often happens and it’s ok. We just have to see what we missed, which is easier now that the branch is more similar to the new base, and apply that on the other two parts. We can then run git-mediate again and have our conflicts resolved.

Had we tried to resolve the conflict manually (without git-mediate) we could have easily missed whether we didn’t apply the full change, and in doing so accidentally revert changes in our conflict resolution. By using git-mediate to verify our work and resolve the conflict for us, we avoid such mistakes.

Using git-mediate is a bit like magic, you do a small little change, press the button, and your conflict is resolved!

A few extra tips:

* Regardless of your merging solution, large merges can often be split into several smaller ones, for example by rebasing feature-branches instead of doing one big merge.
* When figuring out the changes in a conflict isn’t simple, `git-mediate -d` shows, in diff form, the two different changes from the base.

Happy merging!

Discussion:

* <img src="/images/Medium_logo_Monogram.svg" alt="medium" style="width: 20px; display: inline;"/> This post was originally [posted on Medium](https://medium.com/@yairchu/how-git-mediate-made-me-stop-fearing-merge-conflicts-and-start-treating-them-like-an-easy-game-of-a2c71b919984). It was migrated to my new blog at 2020.04.21
* <img src="/images/reddit.svg" alt="reddit" style="width: 20px; display: inline;"/> [r/git](https://www.reddit.com/r/git/comments/5kwrnm/how_gitmediate_made_me_stop_fearing_merge/)