---
title: "Challenges in translating Python to C++"
author: Yair Chuchem
date: 2024.11.08
tags: [code, python, c++]
description: Challenges in translating Python to C++
image: dual-implementation.jpg
draft: []
---

An upcoming product from SR has two implementations:

* The first one is in Python, which allows for quickly iterating to find a solution that works well

* For speed and other considerations, a re-implementation in C++ is required for the actual product

How can one maintain two implementations and make sure that they match?

A common approach is to write tests that compare the outputs of both implementations for a variety of inputs. However, when floating-point calculations are involved, achieving exact matches becomes a challenging task!

For example, consider the simple task of summing an array:

```C++
float sum = 0;
for (float x : arr)
    sum += x;
```

vs

```Python
arr.sum()  # arr is a numpy array
```

At first glance, you might expect both to produce identical results. Surprisingly, they don’t! NumPy employs [pairwise summation](https://en.wikipedia.org/wiki/Pairwise_summation), a more accurate method than the naive C++ loop, leading to differences in the final sum.

So what can we do?

* **Allow approximate matches**: Instead of requiring identical results in tests, verify that the outputs are “close enough” within an acceptable tolerance.
* **Test modules separately**: Test individual components in isolation. Their results are likely to align more closely than those of the entire process, where differences can accumulate.
* **Reuse C++ in Python where possible**: Consider using parts of the C++ implementation directly in Python with [pybind11](https://pybind11.readthedocs.io/en/stable/)
* **Make the implementations more similar**:
  * Implementing pairwise summation in C++ can make its results align better with `np.sum`. This has the added benefit of being more accurate! If the changes don’t require excessive effort or harm performance, this is a worthwhile improvement!
  * Alternatively, using Python's built-in `sum` instead of `np.sum` can produce results identical to the naive C++ loop

When modifying the implementations to align more closely, it’s crucial to respect their respective goals:

* **Python implementation**: Should prioritize developer productivity—keeping the code simple, declarative, and easy to iterate on. Remember, much of the work done in Python may never make it to the final product.
* **C++ implementation**: Should focus on the end user—ensuring the code is fast, robust, portable, and reliable.

### The dream

I desire a single language that would be great for all use-cases. I don't believe that it's inherent that different languages suit better different needs.

If we take static vs dynamic types. I believe that static typing with type inference [can be made ergonomic](https://youtu.be/viF1bVTOO6k?si=Ri8jJbeaghCPFDc1).

I hope one day Lamdu or another similar project will make this dream come true.
