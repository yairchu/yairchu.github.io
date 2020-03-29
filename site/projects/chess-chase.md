---
title: "Chess Chase"
author: Yair Chuchem
date: 2020.03.29
tags: [programming, games, chess, chess-chase, python, kivy, ios]
description: Chess without turns nor sight!
image: Chess Chase.png
---

Chess Chase is a fusion of Chess and modern real-time strategy games like Starcraft.

It is free and [open source](https://github.com/yairchu/chess2).

## Tutorial video

TODO!

## Installing

### macOS or Windows

Download the appropriate installer for your OS [from here](https://github.com/yairchu/chess2/releases).

### Linux/Android/iOS

Build [from the source code](https://github.com/yairchu/chess2).

(The game will soon be available in the iOS app store)

## Rules

### Differences from standard chess

Instead of alternating turns, the game is real-time with cool-downs (aka rate limits):

* After a move, a player cannot move any piece for 0.8 second
* After a piece moves, it cannot move for 2.8 seconds
* A King is quicker and is only frozen for 2.2 seconds
* A Pawn promoted to a Queen becomes frozen for 2.2 seconds as well

Instead of seeing the whole board, you only see these parts:

* The squares that your pieces inhabit
* The squares that your pieces can move to
* The enemy pieces that immediately threaten your King ("Check")
* Enemy pawns that you can capture by [En Passant](https://en.wikipedia.org/wiki/En_passant)

Unlike standard Chess:

* The game is won by actually capturing the King (not before in a [Checkmate](https://en.wikipedia.org/wiki/Chess#Win))
* The King can move to threatened position - so be careful!
* A [Castling](https://en.wikipedia.org/wiki/Castling) can be done regardless of the enemy's attacks - so be careful!
* How [En Passant](https://en.wikipedia.org/wiki/En_passant) relates to turns - if your previous turn happened before the enemy pawn moved you may En Passant capture it.
* There is currently no notion of a tie (but this might change in future update)

### For players not familiar with Chess

Like different units in Starcraft, pieces moves in differently.

You don't need to learn the moves before starting to play, you'll figure it out as you go!

To help you see where pieces can move the board is color-coded to show you where they can move.
For example the "Knight" is assigned the green color and its possibly destinations are colored green.
On a computer, hovering over a piece shows you where it can move.

After a few rounds of the game you will learn how each piece moves, which is the same as their moves in the classic game of Chess!

## Misc

* [Privacy Policy](/projects/chess-chase/privacy-policy)

Similar games:

* [Kung Fu Chess](https://en.wikipedia.org/wiki/Kung-Fu_Chess) - Has the real-time aspect but the whole board is visible
