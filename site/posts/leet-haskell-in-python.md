---
title: "Leet Haskell-style lazy evaluation in Python"
author: Yair Chuchem
date: 2022.09.15
tags: [code, haskell, python, lazy-evaluation]
description: Haskell-style lazy evaluation in Python
image: transparent-snake.jpg
draft: []
---

Haskellers take pride in lazy evaluation, with perhaps its proudest achievement being the world renowned [fibonacci code-golf](https://stackoverflow.com/q/1105765/40916):

```Haskell
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
```

Should Pythoneers envy this majestic implementation? Not anymore!

```Python
@leet
def fibs():
    yield 1
    yield 1
    yield from map(operator.add, fibs(), itertools.islice(fibs(), 1, None))
```

Note that using the `leet` decorator is essential, without it, it would have been terribly inefficient. Here it is:

```Python
def leet(gen):
    """
    Decorate a generator-producing function  
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

Note that the Python version has a significant advantage over the Haskell one, being that the documentation of the `leet` decorator describes an important facet of this algorithm: its memory consumption is far from ideal.

Suppose that you want to iterate over the fibonacci sequence until you find the first item to satisfy some condition. Let's suppose that this value is the 100-billionth value.

Neither this function or the Haskell one it is based on would be useful for this, and you should be advise to use the canonical, constant-memory Python fibonacci implementation instead:

```Python
def boring_fibs():
    cur = next = 1
    while True:
        yield cur
        cur, next = next, cur + next
```

Should you ever prefer to use the leet version? Not really.

Does the leet version have any advantage over the plain one? Possibly that it's cool, depending on who you're asking.

## In defence of lazyness

Devout Haskellers may suggest that the code-golf fibonacci is only a basic demonstration of an idea. A more practical example would be sorting an array.

When you use Haskell's `sort` function but only consume the first two elements of the result, it doesn't need to sort everything and you get an O(n) of work rather than O(n * log(n)).

However you could implement it in Python as well:

```Python
def lazy_quick_sort(gen):
    """
    Haskell-style lazy quick-sort.
    It doesn't need to fully sort everything to find the N smallest elements.

    The complexity would be O(N*log(K)) where K is the ammount iterated,
    in comparison to O(N*log(N)) for the full sorting algorithm.

    An example use case is iterating over potential dates
    ordered by their level of attractiveness,
    until one of them agrees to date you.
    (but be warned that the odds for that may be lower
     if you tend to use functions like this one)
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
    yield from quick_sort(less)
    yield pivot
    yield from quick_sort(more)
```

## Notes

Header image was generated with DALL-E 2 and Photoshop combo.

* I first asked DALL-E for "a cute friendly floating snake built of caterpillar treads in a natural environment, 3d render"
* Then I edited the first version to remove the snake, with the prompot "natural environment, 3d render". Note that without removing the snake's reflection in the water DALL-E would bring it back rather than making a backround.
* In Photo-shop I overlaid the image with the snake on the background image adding a gradient mask to make its tail fade to transparency, which is my artistic interpretation of lazy evaluation in Python