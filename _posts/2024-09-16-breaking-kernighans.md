---
layout: post
title: Breaking the Kernighan's Law
category: Lisp
permalink: /breaking-the-kernighans-law/
---

<div class='epigraph'>
*"Debugging is twice as hard as writing the code in the first place.
  Therefore, if you write the code as cleverly as possible, you are,
  by definition, not smart enough to debug it.."* --- Brian W. Kernighan.
</div>

I'm a sucker for sage advice much as anyone else, and Kernighan is certainly right on money in the epigraph. Alas there comes a time in programmer's career when you just end up there despite the warning. It could be that you were indeed too clever for your own good, or maybe the code isn't quite yours anymore after each of your colleague's take on it over the years. Or just sometimes, the problem is indeed so hard that it strains your capacity as a coder.

It would usually start with a reasonable idea made into first iteration code. The solution looks fundamentally sound but then as you explore the problem space further it begins to seep *nuance*, either as manifestation of some real world complexity or your lack of foresight. When I run into this my first instinct is to instrument the code. If the problem is formidable you got to respect it: flailing around blindly modifying things or ugh, doing a rewrite at this stage is almost guaranteed to be a waste of time. It helps to find a promising spot, chisel it, gain a foothold in the problem, and repeat until you crack it. Comfortable debugging tools here can really help to erode the original Kernighan coefficient from 2 to maybe 1.6 or 1.4 where you can still have a chance.

Lisp users are fortunate with the options of interactive debugging, and one facility I reach often for is the plain `BREAK`. It's easy enough to wrap it into a conditional for particular matches you want to debug. However sometimes you want it to trigger after a particular sequence of events across different positions in code has taken place. While still doable it quickly becomes cumbersome and this state machine starts to occupy too much mental space which is already scarce. So one day, partly as a displacement activity from being intimidated by a Really Hard Problem I wrote down my debugging patterns as a handful of macros.

Enter [BRAKE](https://github.com/varjagg/brake). Its features reflect my personal preferences so are not necessarily your cup of tea but it could be a starting point to explore in this direction. Things it can do:

 - act as a simple `BREAK` with no arguments (duh)
 - wrap an s-expression, passing through its values upon continuing
 - trigger sequentially based on the specified position for a common tag
 - allow for marks that don't trigger the break but mark the position as reached
 - provide conditional versions for the expressions above
 - print traces of tagged breakpoints/marks

If you compile functions with debug on you hopefully should be able to see the wrapped sexpr's result values.

{% highlight lisp %}
(use-package '(brake))

(defun fizzbuzz ()
  (loop for n from 100 downto 0
	for fizz = (zerop (mod n 3))
	for buzz = (zerop (mod n 5)) do
	(format t "~a "
		(if (not (or fizz buzz))
		    (format nil "~d" n)
		  (brake-when (= n 0)
			      (concatenate 'string
					   (if fizz "Fizz" "")
					   (if buzz "Buzz" "")))))))
{% endhighlight %}

These macros try to detect common cases for tagged sequences being either aborted via break or completed to the last step, resetting them after to the initial state. However it is possible for a sequence to end up "abandoned", which can be cleaned up by a manual command.

Say in the example below we want to break when the two first branches were triggered in a specific order. The sequence of 1, 3, 4 will reinitialize once the state 4 is reached, allowing to trigger continuously. At the same time if we blow our stack it should reset to initial when aborting.

{% highlight lisp %}
(defun ack (m n)
  (cond ((zerop m) (mark :ack 3 (1+ n)))
        ((zerop n) (mark :ack 1 (ack (1- m) 1)))
        (t (brake :ack 4 (ack (1- m) (ack m (1- n)))))))
{% endhighlight %}

In addition there are a few utility functions to report on the state of brakepoints, enable or disable brakes based on tags and turn tracing on or off. Tracing isn't meant to replace the semantics of `TRACE` but to provide a souped up version of debug by print statements everyone loves.

{% highlight lisp %}
CL-USER> (report-brakes)
Tag :M is DISABLED, traced, with 3 defined steps, current state is initial
Tag :F is DISABLED with 2 defined steps, current state is 0
Tag :ACK is ENABLED with 3 defined steps, current state is initial
{% endhighlight %}

Disabling breakpoints without recompilation is really handy and something I find using all the time. The ability to wrap a sexpr was often sorely missed when using `BREAK` in constructs without implicit body.

Sequencing across threads is sketchy as the code isn't guarded but in many cases it can work, and the appeal of it in debugging races is clear. One of those days I hope to make it more robust while avoiding potential deadlocks but it isn't there yet. Where it already shines tho is in debugging complex iterations, mutually recursive functions and state machines.
