---
title: "Chess Chase's Privacy Policy"
author: Yair Chuchem
date: 2020.03.29
tags: [chess-chase, ios, apple, bureaucracy]
description: The privacy policy of chess-chase
image: various-business-office.jpg
---

When playing Chess Chase, the game's communication is peer-to-peer, not involving any servers except when establishing the connection.

There are several steps involved in establishing the connection:

* First, the game first contacts a third-party [STUN service](https://en.wikipedia.org/wiki/STUN)
  to find out the game's internet IP address and [UDP](https://en.wikipedia.org/wiki/User_Datagram_Protocol) port.
  One of [the providers used](https://github.com/talkiq/pystun3/blob/master/stun/__init__.py#L10)
  by the [`pystun3`](https://github.com/talkiq/pystun3) library is picked at random.
  There is no personal information involved in this service and the third-party providers
  do not know if you are playing a game or conducting a VoIP call or other p2p task.
* When it knows its IP, the game registers it with the
  [matching server](https://github.com/yairchu/game-match-server),
  which provides the service of matching peers by a short random phrase, such as "BEER BANG DEER".
  The matching server gets no personal information except for the game's internet IP address
  which is essential for the communication to occur.

All of the in-game chat and plays are never transmitted to any server and are sent to the other player via direct peer-to-peer communication.

The servers only know your IP addresses and whether you tried to establish a connection with a peer.
This information is essential to establish the connection.
This information may also be used to create aggregate statistics which count number of games played,
and your IP will never be disclosed to any other party.

Image source: https://www.wallpaperflare.com/office-stapler-and-paperclip-various-business-close-up-white-wallpaper-woypt
