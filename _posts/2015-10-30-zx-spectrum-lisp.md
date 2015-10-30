---
layout: post
title: Serious Software SpecLisp for Sinclair ZX Spectrum
category: Lisp
---

The first computer I owned and learnt the basics of programming on was a [Sinclair ZX Spectrum](https://en.wikipedia.org/wiki/ZX_Spectrum) clone. Very modestly specced machine even for its heyday, it nonetheless could run a range of *high level languages* aside from built-in BASIC. I remember successfully porting backpropagating network code from German [C't](http://www.heise.de/ct/) magazine to HiSoft Pascal, and fumbling around in HiSoft C.

Seirous Software SpecLisp, however, was a bit of enigma. Lisp tutorials were relatively hard to come by in pre-Internet age, and naive tinkering led nowhere.

![Booting up](/images/zx-spectrum-lisp/1.gif)

Fast forward to 2015, the days of information abundance, when things like [SpecLsp user guide](http://www.spectrum20.org/reviews/3494) are floating around. The interpreter can be downloaded off the net as well, and ran under a number of free emulators. Let's finally have a look.

The origins of this dialect can be immediately traced to Lisp 1.5, down to the use of plist markers such as `PNAME` and `APVAL`, and rather specific `(oblist)` function, serving the original function of dumping the definitions in memory. As expected, it uses dynamic binding and *FUNARG problem* is not quite solved. And no `QUOTE` shortcuts: you have to spell it like you mean it.

Just like Lisp 1.5, it comes from the times predating bike helmets, mandatory seatbelts and interpreter sandboxes. Native function definitions are stored as call addresses via `SUBR` property of symbol plists. As such it perhaps makes the most concise FFI ever. Here's it issuing a system reset by jumping to 0x0000. I dare you to do that with a modern Lisp!

![Reset](/images/zx-spectrum-lisp/2.gif)

Being a true Lisp interpreter, it allows one to edit lambda definitions equally trivially:

![Modify lambda](/images/zx-spectrum-lisp/3.gif)

Now, it's easy to rationalize how self-modifying code is not really a feature and there's no need for it in the age of macros. But admit it, this is still kind of neat, in its own old school way. Feels just right. Besides, there's no other way to edit the definitions..

Remarkably, SpecLisp is case sensitive and lowercase by deafault, predating Franz bold introduction of that to the world of Common Lisp by 17 years.

![Case sensitivity](/images/zx-spectrum-lisp/4.gif)

But perhaps the most surprising part is SpecLisp's graphics extension. The interpreter was probably meant to serve educational purposes (even by 1983 standards, ZX Spectrum was an underwhelming Lisp machine), hence it featured a set of Logo-like turtle control commands.

Let's put it to good use by making a period true graphics demo.

The initialization routine to clear screen and center the pen:

{% highlight lisp %}
(de prep (a)
  (progn nil
	(cls)
	(fwd 88) (right 90) (fwd 128) (right 270)
	(pend)))
{% endhighlight %}

A function to render a square of specified size, and another to draw those squares in diminishing size while rotating:

{% highlight lisp %}
(de square (n)
  (progn nil
	(fwd n) (right 90)
	(fwd n) (right 90)
	(fwd n) (right 90)
	(fwd n) (right 90)))

(de rotate (a n c)
  (while (greaterp n 0)
	(right a)
	(square n)
	(setq n (diff n c))))
{% endhighlight %}

In case you wondered, the opening `nil` in `progn` is the empty bindings list: the construct doubles as impromptu `let`.

![Diminishing squares](/images/zx-spectrum-lisp/5.gif)
