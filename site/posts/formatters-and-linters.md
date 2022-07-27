---
title: "Modern coding with formatters and linters"
author: Yair Chuchem
date: 2022.07.26
tags: [code, python, rust, lamdu]
description: Modern coding with formatters and linters and how Lamdu does it differently
image: hal-light-bulb.jpg
---

Many developers nowadays use code auto-formatters and linters. Modern languages like Rust even have these tools built in with their environments (`cargo fmt`, `cargo clippy`).

You may write a piece of Python code like

```Python
if a:
    something_long(that_should_be_split, to_several_lines, because_we_dont, like_horizontal_scrolling)
else:
    if b:
        c()
    else:
        d()
```

And then click a button to auto-format it to:

```Python
if a:
    something_long(
        that_should_be_split,
        to_several_lines,
        because_we_dont,
        like_horizontal_scrolling,
    )
else:
    if b:
        c()
    else:
        d()
```

Afterwards you may handle linter suggestions from a tool such as [sourcery](https://sourcery.ai) to turn it to:

```Python
if a:
    something_long(
        that_should_be_split,
        to_several_lines,
        because_we_dont,
        like_horizontal_scrolling,
    )
elif b:
    c()
else:
    d()
```

This is certainly an improvement over eras when those suggestions came from manual code reviewers work in pull requests.

But I can't help but notice how many steps are in the programming process:

![Programmer State Machine](/images/programmer-state-machine-2.png)

Wouldn't it be awesome if this was a non-issue? I think it would, and that's how it works in [Lamdu](http://www.lamdu.org):

* Code layout is automatic as you edit. Formatting happens in real-time and doesn't require an extra step. It's even responsive and will break lines differently depending on the available width.
* Syntax sugars like `elif` apply automatically whenever the code matches the pattern, always. We still aim to get out of your way if you want to make changes that would not fit the sugar anymore, and it will automatically switch back to unsugared form.

For more info on Lamdu's approach see the demo in this video:

<iframe style="display: block; margin: auto; margin-top: 15px;" width="640" height="360" src="https://www.youtube.com/embed/Q61dh87WGrE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen
></iframe>

## Notes

* The back-end deployment state-machine adds even more steps to the development process and [Dark](https://darklang.com) is taking steps to simplify this
* Header image: Scene of [Hal fixing a light-bulb](https://youtu.be/AbSehcT19u0)  from Malcolm in the Middle
