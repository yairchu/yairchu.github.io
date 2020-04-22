---
title: "Break big merges to smaller pieces"
author: Yair Chuchem
date: 2020.04.22
tags: [code, git, merge, rebase]
description: Tactics for merging in smaller chunks
image: shape-sorter.jpg
---

When I have a merges with many conflicts I become stressed.

While the chance of making a mistake in resolution increases, I can't run the test suite to verify my work until all of them are resolved.

Often however we can break the merge process down to smaller pieces, where we can check and save our work after each step.

![Git merge](/images/merge.svg)

The simplest way to break a merge down is to apply "sub-merges" which merge a single commit at a time.

![Git merge with sub-merges](/images/sub-merge.svg)

Each sub-merge involves less conflicts and we can run our test-suite to verify ourselves.

A down-side of this simple approach is that it may be tedious to do manually and it will result in a very complicated git history tree.

One way to resolve the history issue is to rewrite it, which one can do with `git rebase`.

If rebasing rather than merging, the following script makes the process easy by automating it:

## sub-rebase.sh

```Bash
BASE=$1
if [ "$BASE" == "" ]
then
    BASE=master
fi
while true
do
    NEXT_COMMIT=`git rev-list ..$BASE | tail -n 1`
    if [ "$NEXT_COMMIT" == "" ]
    then
        echo "Done"
        exit 1
    fi
    git rebase $NEXT_COMMIT || exit 1
done
```

`sub-rebase.sh` applies a rebase up to the first parent of the base branch which has merge conflicts to address.

Apply it to advance towards your complete merge in smaller, testable pieces.

Notes:

* For resolving the conflicts, [I recommend using git-mediate](/posts/git-mediate-stops-fear)
* Image from [this meme](https://www.reddit.com/r/funny/comments/ub7x3/fail_shape_sorter_college_campus_level/)
