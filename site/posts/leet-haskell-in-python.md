---
title: "Leet Haskell-style lazy evaluation in Python"
author: Yair Chuchem
date: 2022.09.15
tags: [code, haskell, python, lazy-evaluation]
description: Haskell-style lazy evaluation in Python
image: python-embracing-sloth.jpg
---

Haskellers take pride in lazy evaluation, with the world renowned Fibonacci sequence code-golf as one of its proudest achievements:

```Haskell
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
```

Should Pythoneers envy this majestic implementation? Not anymore:

```Python
@leet
def fibs():
    yield 1
    yield 1
    yield from map(operator.add, fibs(), itertools.islice(fibs(), 1, None))
```

The usage of the `leet` decorator above is essential. Without it `fibs` would have been terribly inefficient. Here's how this decorator works:

```Python
def leet(gen):
    """
    Decorate a function returning a generator
    to memoize its consumed values for all eternity.
    """
    original = gen()
    as_list = []
    def result():
        for i in itertools.count():
            if i == len(as_list):
                as_list.append(next(original))
            yield as_list[i]
    return result
```

Note that the documentation of `leet` clearly explains an important facet of this approach: just like the Haskell version, its memory consumption is far from ideal.

Suppose that you want to iterate over the fibonacci sequence until you find the first item to satisfy some condition. If this value is the 100-billionth value your program will run out of memory before it reaches it. Instead you should be advised to use the canonical, constant-memory fibonacci implementation:

```Python
def boring_fibs():
    cur = next = 1
    while True:
        yield cur
        cur, next = next, cur + next
```

Should you ever prefer to use the leet version over the boring one? Probably not. So does the leet version have any advantage? Maybe that it is kind of cool, subjectively speaking? I guess not.

## In defence of lazyness

Devout Haskellers may suggest that the code-golf fibonacci is only a basic demonstration of the idea. A classic more practical use-case is sorting arrays.

When you use Haskell's `sort` function but only consume the first two elements of the result, it doesn't need to sort everything and you get an O(n) of work rather than O(n * log n).

However you could implement it in Python as well:

```Python
def lazy_quick_sort(gen):
    """
    Haskell-style lazy quick-sort.
    It doesn't need to fully sort everything to find the N smallest elements.

    The complexity would be O(N * log K) where K is the ammount iterated,
    in comparison to O(N * log N) for the full sorting algorithm.

    The canonical example use-case is iterating over potential dating candidates
    ordered by their level of attractiveness,
    until finding one that will agree to date you.
    """
    gen = iter(gen)
    try:
        pivot = next(gen)
    except StopIteration:
        return
    less = []
    more = []
    for x in gen:
        (less if x < pivot else more).append(x)
    yield from lazy_quick_sort(less)
    yield pivot
    yield from lazy_quick_sort(more)
```

## My conclusions

While lazy evaluation is very useful, the pervasive lazy evaluation in Haskell (rather than explicit lazy evaluation in other languages) probably causes more trouble than it's worth. In the fibonacci example it leaks memory, and people [have to go to great lengths to work around it](https://www.reddit.com/r/haskell/comments/2g9akh/preventing_memoization_in_ai_search_problems/), and while in the sorting example it does reduce the time complexity from O(log N) to O(log K), the overhead added by the language runtime to implement pervasive lazyness probably adds a larger factor than the one saved.

If I were to design a programming language ([which I am](http://www.lamdu.org)) then I would choose to not have pervasive lazy evaluation, but to make an effort to design the language to accomodate explicit lazy evaluation ergonomically.

## Notes

* Header image was generated with DALL-E with the prompt "A cute friendly python embracing a sloth, digital art"
