---
title: "Chess Chase"
author: Yair Chuchem
date: 2020.03.29
tags: [code, games, chess, chess-chase, python, kivy, ios]
description: Chess without turns nor sight!
image: Chess Chase.png
---

Chess Chase is a fusion of Chess and modern real-time strategy games like Starcraft.

It is free and [open source](https://github.com/yairchu/chess2).

## Tutorial video

<iframe style="display: block; margin: auto; margin-top: 15px;" width="640" height="360" src="https://www.youtube.com/embed/9XRKH32EaWQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen
></iframe>

## Installing

### macOS or Windows

Download the appropriate installer for your operating system [from here](https://github.com/yairchu/chess2/releases).

### iOS

The game is currently [available for beta-testing](https://testflight.apple.com/join/dpRLg7th) via Apple's TestFlight. It is not yet available in the normal App Store.

### Linux/Android

Build [from the source code](https://github.com/yairchu/chess2).

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
* The King can move to threatened positions - so be careful!
* [Castling](https://en.wikipedia.org/wiki/Castling) can be done regardless of the enemy's attacks - so be careful!
* How [En Passant](https://en.wikipedia.org/wiki/En_passant) relates to turns - if your previous turn happened before the enemy pawn moved you may En Passant capture it.
* There is currently no notion of a tie (but this might change in future updates)

### For players not familiar with Chess

Like in Starcraft, there are different kind of units and each kind moves in different ways.

You don't need to learn the moves before starting to play, you'll figure it out as you go!

To help you see where pieces can move, the board is color-coded.
For example, the "Knight" is assigned the green color and its possibly destinations are colored green.
On a computer, hovering over a piece shows you where it can move.

After playing a few rounds, you will learn how each piece moves, which is the same way it moves in the classic game of Chess!

## Misc

* [The Chess piece graphics](https://commons.wikimedia.org/wiki/File:Chess_Pieces_Sprite.svg) were created by Cburnett and jurgenwesterhof
* [Privacy Policy](/projects/chess-chase/privacy-policy)
* Similar game: [Kung Fu Chess](https://en.wikipedia.org/wiki/Kung-Fu_Chess). It has the real-time aspect but the whole board is visible.
* Feedback is most welcome! It can be given via Twitter, GitHub issues, or the medium of your choice.
