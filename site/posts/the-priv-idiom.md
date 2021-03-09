---
title: "The C++ Priv idiom: an alternative to Pimpl"
author: Yair Chuchem
date: 2021.03.09
tags: [code, c++]
description: Priv - an alternative to the C++ Pimpl idiom
image: CoolCLIPS_vc062452.jpg
---

C++ is infamous for long compilation times.

Part of the problem is that editing `private` declarations of a class causes recompilation for all of its users.

There are several ways to work around this problem and reduce incremental compile times:

* Use interfaces. A lot of code using a concrete class could use an interface instead. This comes with a run-time price of using virtual functions.
* [The Pimpl](https://stackoverflow.com/questions/8972588/is-the-pimpl-idiom-really-used-in-practice) idiom (private pointer to implementation), achieves the same compile-time benefit as interfaces without the virtual calls, but comes at a run-time price of allocating and using additional heap objects.
* The Priv idiom, introduced below, does not sacrifice runtime performance at all, but it also only brings part of the compilation time benefits of the other approaches.

Below is a short introduction to `Pimpl` and `Priv`, some benchmark results, and closing commentary.

## Illustrating Pimpl with a simple example

```C++
class SpamFilter
{
public:
    SpamFilter (const std::set<std::string>& forbiddenWords);
    ~SpamFilter();

    bool isSpam (const std::vector<std::string>& words) const;

private:
    class Impl;

    std::unique_ptr<Impl> impl;
};
```

Using the Pimpl idiom, our class doesn't have any `private` declarations
apart from the internal `Impl` class and `impl` member.

The actual `SpamFilter::Impl` class is defined in the `.cpp` file and does all of the work.

The benefit of this approach is that when we change implementation details, no recompilation of other modules will trigger.

The down-sides are:

* Performance is sacrificed due to forcing additional heap allocations (for the `Impl` object) and additional indirections.
* Boiler-plate, public methods of the class just call the same method of the `Impl` class.

## Priv

`Priv` is an alternative idiom without the down-sides of Pimpl, but also with only a part of its benefit, as we do declare the private members normally:

```C++
class SpamFilter
{
public:
    SpamFilter (const std::set<std::string>& forbiddenWords);
    bool isSpam (const std::vector<std::string>& words) const;

private:
    Struct Priv;

    std::set<std::string> m_forbiddenWords;
};
```

No private methods are declared in the `.h` file. According to this idiom they all belong to the `Priv` subclass, declared in the `.cpp` file:

```C++
struct SpamFilter::Priv
{
    static bool isSpam (const SpamFilter&, const std::string& word);
};
```

The private methods turned into static methods of the `Priv` nested class, so we use them like so:

```C++
bool SpamFilter::isSpam (const std::vector<std::string>& words) const
{
    for (const auto& x : words)
        if (Priv::isSpam (*this, x))
            return true;
    return false;
}
```
## Comparison of idioms

<table style="text-align: center">
<tr>
    <td></td>
    <th>Boilerplate</th>
    <th>Slowdown</th>
    <th>Extra Recompilations</th>
</tr>
<tr>
    <th style="text-align: right">Plain</th>
    <td class=green-bg colspan=2>None (baseline)</td>
    <td class=red-bg>On any change</td>
</tr>
<tr>
    <th style="text-align: right">Interface</th>
    <td class=red-bg>+37%</td>
    <td class=yellow-bg>less than 1%</td>
    <td class=green-bg rowspan=2>When the interface changes</td>
</tr>
<tr>
    <th style="text-align: right">Pimpl</th>
    <td class=red-bg>+53%</td>
    <td class=red-bg>10%</td>
</tr>
<tr>
    <th style="text-align: right">Priv</th>
    <td class=yellow-bg>+13%</td>
    <td class=green-bg>None</td>
    <td class=yellow-bg>Also when private members change</td>
</tr>
</table>

The comparison numbers are based on a [simple benchmark](https://github.com/yairchu/cpp-idiom-bench) (run-time was measured using `time` on a 2020 M1 Macbook Pro)

Should one use any of these idioms just to reduce compilation times? As C++ already has plenty of boiler-plate with header files, I'd be relunctant to add more. And why are we even using C++ in the first place if not to achieve the best run-time performance? Therefore I would prefer not to use the Pimpl idiom, and consider Priv over it, but also after exausting any other approaches to reduce compilation times without any weird workaround (like using forward declarations and [include-what-you-use](https://include-what-you-use.org)).

## Notes

<a href="https://pajam.live/"><image alt="pajam!" src="/images/pajam-icon.svg" width="75px" /></a>

* The Priv idiom as presented here is an improvement over my original formulation, and was suggested by [9cantthinkofgoodname on reddit](https://www.reddit.com/r/cpp/comments/m15i86/the_priv_idiom_an_alternative_to_pimpl/gqbpzx7/)
* This post was written in response of Pimpl being used in the code-base of [Pajam](https://pajam.live/), and I intend to present it to my colleagues to discuss the trade-offs of this idiom. Will update on the results!
* Regardless of whether it uses the optimal internal implementation idioms, [Pajam is really cool!](https://youtu.be/ahTbPlTtuuw) If you want to jam musically with your remote friends, be sure to try it out!
* Title image credit: [CoolClips.com](http://search.coolclips.com/m/vector/vc062452/sweeping-it-under-the-rug/)
* <img src="/images/reddit.svg" alt="reddit" style="width: 20px; display: inline;"/> [r/cpp discussion](https://www.reddit.com/r/cpp/comments/m15i86/the_priv_idiom_an_alternative_to_pimpl/)
