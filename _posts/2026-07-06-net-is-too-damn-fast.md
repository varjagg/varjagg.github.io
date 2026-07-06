---
layout: post
title: The Net is Too Damn Fast
category: Lisp
---
Recently I stumbled on a funny kind of race in distributed systems. I believe even the [classic texts](https://en.wikipedia.org/wiki/Fallacies_of_distributed_computing) don't cover that.

So say we have a system `S` sending commands to a receiver `R` over network. `S` maintains network outbox and inbox handled by separate threads. A command is expected to complete with certain result sent back before the deadline, or else `S` would assume a request timed out. Quite basic so far.

However a certain class of commands (and only it) was timing out. They would fail regularly on some networks, sporadically on others and seemingly never on some. Perplexingly they were really simple commands, amounting to little else than sending back a reading of `R`'s internal state. Increasing the command deadline had no apparent effect. Adding logging in places helped little and in fact often resulted in the issue disappearing.

Simplifying it quite a bit the ~S~ side looked something like this:
{% highlight lisp linenos %}
(defun handle-outbox (socket queue)
  (let ((command (pop queue)))
    (handler-case (send-command socket command))
      (network-error (c)
	(log-network-error c))
      (:no-error (c)
	(register-command-awaiting-response command))))

(defun handle-inbox (socket)
  (let ((reply (receive-incoming socket))
	(corresponding-command (command-awaiting-response response)))
    (when corresponding-command
      (process-reply-for-command command response))))


{% endhighlight %}

You see now what was happening: `S` was sending the command, `R` processing it, sending the reply and `S` handling the reply *before* the bookkeeping of outbox process would record the command. Then the response wouldn't match anything and be discarded as orphaned. Later the inbox would timeout the command (not shown).

The bookkeeping wasn't even that heavyweight: just storing some structures and perhaps couple expensive calls to set up a condition variable but that was enough.

Naturally any kind of latency (be it due to network load or extraneous syslog calls) would alleviate that. The fix was to make the code inelegant but correct by registering the command before the send attempt and un-registering in the event of failure.

*tl;dr a network can be way too fast*
