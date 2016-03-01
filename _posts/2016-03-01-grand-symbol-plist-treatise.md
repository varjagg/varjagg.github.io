---
layout: post
title: The Grand SYMBOL-PLIST Treatise
category: Lisp
---

Plists are handy simple key/value stucures, stored in a single-linked list. A plist `(a 1 b 2 c 3)` holds three numeric *values* for *properties* a through c. Lisp dialects have standard functions for conveniently adding, looking up and removing the properties.

All operations are thus O(n), but shorter plists (say a dozen or two entries) typically beat hash tables due to substantial constant overhead of the latter.

Symbol-plists specifically are plists attached to a Lisp symbol via dedicated language mechanism.

### History

The earliest mentions of property lists predate first Lisp manuals. In his 1958 _Programs with Common Sense_ paper[1], John McCarthy had to say the following:

> For example, to most people, the number 3812 is not an object: they have nothing to say about it except what can be deduced from its structure. On the other hand, to most Americans the number 1776 is an object because they have filed somewhere the fact that it represents the year when the American Revolution started. In the *advice taker* each object has a *property list* in which are listed the specific things we have to say about it.

Some remarkable facts can be deduced from this short passage:

* Symbol-plists predate Lisp symbols conceptually. Here, a number is used as an object with a list of properties.
* The intent is providing a form of associative memory, a key-value store for the procedural evaluator. It's like noSQL before SQL!
* Symbol-plists are the original sin of functional programming. They were meant for side effect in FP before FP was a thing. Thus classic Lisp was never meant to be purely functional; state is not an oversight but a deliberate design decision.
* Even though the precise storage model is unspecified, a list is implied. LISP at the time was still a study in progress and hash tables were known since 1953, but they would have been too costly in terms of memory.

### The earliest known object orientation mechanism

The use of word *object* in the quote above is not coincidental. The sets of properties were defining the object entities. The objects in early Lisp would manifest as symbols. LISP 1.5 implementation hinged on symbol-plists, with four pre-defined properties:

* `PNAME`, the print name of the symbol
* `EXPR`, a user defined associated function, stored as S-expression
* `SUBR`, pointer to machine code built in subroutine
* `APVAL`, the global scope storage, confusingly referred as "constant value" (as opposed to value within the function's dynamic scope)

This scheme was later split into symbol-name, symbol-function and symbol-value cells.

Plists and symbols were thus providing

* User definable object properties
* Encapsulation mechanism with accessors
* Pre-defined system properties
* Dedicated property for associated function

Granted, there was no explicit `this`/self-reference to be used in stored function, but that's just sugar trivia:

{% highlight cl %}
(defmacro mydefun (name args &rest body)
  `(defun ,name ,args
	(let ((this ',name))
	  ,@body)))

(mydefun foo ()
  this)

(foo) => FOO
{% endhighlight %}

..the reason it's not there in the spec is that the "modern" OO model was not a design concern. *Advice Taker* was meant to be procedural logic manipulator, not an agency. This conceptual difference would lead to eventual split of McCarthy-Minsky cooperation, and we'll see Minsky introducing the object-referencing methods with his *frames* effort.

It is important though that Lisp symbols were true object entities existing at run-time from day zero.

### Symbol-plists as namespaces

Symbol-plists can be thought as defining factor of lisp arity. In the degraded case of Scheme we get lisp-1 with one name/value pair only.

Lisp 1.5 is an lisp-n, where `n` is the size of the largest symbol plist in the system.

Common Lisp is a lisp-(c + n), where `c` is implementation-dependent number of key-value pairs (or *cells*) factored out of original symbol-plist. Funny enough, symbol-plist is now one of the 4 standard mandated cells itself. The advantage here is the system cells are safe from accidental plist smashing and don't impose a run-time penalty for user's plist manipulating code.

### Use and disuse

Nearly all early AI programs make extensive use of symbol-plists. As a didactic sample I would recommend annotated tree implemenation in Steve Tanimoto's [ID3-like inducer](ftp://ftp.cs.washington.edu/homes/tanimoto/ai/INDUCTR.CL). However, with gradual changes in Lisp implemenation landscape and programming mindset, symbol-plists fell into disuse.

An obvious contributing factor to symbol-plists decline was the removal of system properties into separate cells. However, the bigger affecting trend was the move from symbolic manipulation in Lisp code throughout 1980-1990s. This had several reasons. A symbol manipulating, computationalist AI effort has largely collapsed facing advances in statistical ML techniques. The proliferation of viable macro systems and native compilation obliterated demand to code-as-data manipulation. This was amplified by the influx of Algol tradition programmers, where symbol entities are simply not a concept and hence the whole approach remained alien.

There is still acceptance of detached plists for transient key/value records, but unsurprisingly, symbol-plists are viewed as obsolete, almost counter-cultural thing. Now it's certainly personal, but plists without symbols are like naked singularities: they might occur in nature but still are abominations. Or like garden slugs: sad, vulnerable version of a snail without a place to belong. As plists have no inherent type in Common Lisp[2], their interpretation is contextual, and outside of symbol-plist realm the context is not necessarily clear.


[1] Published in vol.1 1959 proceedings of _Mechanisation of Thought Processes_ conference. A remarkable print from low hanging fruit times of Computer Science, with submissions from McCarthy, Minsky, Ershov, Hopper, Backus, McCulloh and other pioneers of note.

[2] Interestingly, in Lisp 1.5 plists were tagged by -1 value in CAR of the list. There were other storage optimizing techniques too, such as special format for *flags*, the valueless properties.
