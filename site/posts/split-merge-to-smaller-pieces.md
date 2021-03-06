---
title: "Break big merges to smaller pieces"
author: Yair Chuchem
date: 2020.04.22
tags: [code, git, merge, rebase]
description: Tactics for merging in smaller chunks
image: shape-sorter.jpg
---

Merges with many conflicts are horrifying.

As the chance for a conflict resolution mistake increases, we can't run the tests to verify correctness until all of the conflicts are resolved.

Often, however, we can break the merge process down to smaller pieces, where we can check and save our work after each step!

![Git merge](/images/merge.svg)

The simplest way to break a merge down is to apply "sub-merges" which merge a single commit at a time.

![Git merge with sub-merges](/images/sub-merge.svg)

Each sub-merge involves less conflicts and we can run our test-suite to verify ourselves.

A down-side of this simple approach is that it may be tedious to do manually and it will result in a very complicated git history tree.

One way to resolve the history issue is to rewrite it, which one can do with `git rebase`.

If we rebase rather than merge, the following script makes the process easy by automating it:

## sub-rebase.sh

```Bash
#!/bin/bash

BASE=${1:-master}
while true
do
    # Find next commit to rebase to
    NEXT_COMMIT=
    COMMON_ANCESTOR=$(git merge-base HEAD $BASE)
    for COMMIT in $(git rev-list ..$BASE --reverse)
    do
        if [ $(git merge-base HEAD $COMMIT) == $COMMON_ANCESTOR ]
        then
            # Rebasing on this commit will strictly progress towards our goal
            NEXT_COMMIT=$COMMIT
            break
        fi
        # The commit is not strictly ahead of BASE
        echo Skipping $COMMIT as it is not strictly ahead of $BASE
    done

    [ "$NEXT_COMMIT" == "" ] && echo "Done" && exit
    echo Rebasing over $NEXT_COMMIT
    git rebase $NEXT_COMMIT || exit 1
done
```

It rebases up to the first parent of the base branch which has any merge conflicts to address (so it doesn't accumulate conflicts from multiple commits).

Apply it to advance towards your complete merge in smaller, testable pieces.

*Update:* I learned about the [`git-imerge` tool](https://github.com/mhagger/git-imerge) which aims to do exactly what I suggest here. I will give it a try and will update this post subsequently!

Notes:

* This process works well when we commit often in small commits.
* For resolving the conflicts, [I recommend using git-mediate](/posts/git-mediate-stops-fear)
* Image from [this meme](https://www.reddit.com/r/funny/comments/ub7x3/fail_shape_sorter_college_campus_level/)

Updates:

* 2020.05.19: Updated script to skip over commits that are not strictly ahead of the common ancestor with the base branch
