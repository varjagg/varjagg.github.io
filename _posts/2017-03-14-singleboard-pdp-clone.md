---
layout: post
title: MS1201.02 single-board PDP-11 clone
category: Hadrware Vintage PDP
---

Among the first computer related-books I read was a PDP-11 programmer's manual, borrowed in a library. It was a bit too dense for someone who never seen a computer in their life. Save for the random bits like bus arbitrage or advantages of magnetic domain memory over core, nothing stuck really. A bit later came my first real programming experience, a few simple BASIC programs on [DVK](https://en.wikipedia.org/wiki/DVK), a DEC PDP-11 clone, under RT-11.

Then this winter, browsing eBay junkyards, I came across an MS-1201.02: a Q-Bus board at heart of DVK. A couple weeks later, it was on my desk:

![MS1201.02](/images/pdp_11/1.jpg)

Looks like a finest specimen of Soviet electronic manufacture, but robbed of its KM type platinum/palladium capacitors by precious metal scavengers. It's a wonder that the processor (16-bit K1801VM2) in gold-plated contact CERDIP package wasn't removed. The CPU has a crack in the package however, which might prevent it's function.

The board has three jumper blocks for configuration. Or at least it had in theory.. in reality, communist DIP jumpers were so bad the factories stopped installing them after a while. So if you wanted to change a serial port speed or boot address, you had to do it like Real Men do: with bare hands and soldering iron.

![Jumper block](/images/pdp_11/3.jpg)

The board has 64Kb physical memory, only 56Kb (28 Kwords) of which are however addressable - the upper 8Kb are I/O mappings in PDP-11 architecture.

So as you see, some work to be done here. Immediate plan is:

* Order all the missing caps and repopulate them
* Find a replacement CPU chip and replace if necessary
* Install less manly, capitalist DIP piano switch blocks
* Build a backplane to install the card in
* Build a current loop serial interface to RS-232c converter
* As the board uses two "power good" signals in startup sequence, build a small flip circuit for start-up
* Boot the board to the built in serial monitor prompt

Out of that, the caps, CPU and backplane components already ordered. Schematics are fortunately available on the net.

To be continued.

