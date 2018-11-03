---
layout: post
title: Some documents on AM and EURISKO
category: Lisp
---

Sharing here a small collection of documents by Douglas B. Lenat related to design AM and EURISKO that I assembled over the years. These are among the most famous programs of symbolic AI era. They represent so-called 'discovery systems'. Unlike expert systems, they run loosely-constrained heuristic search in a complex problem domain.

AM was Lenat's [doctoral thesis](/docs/am-eurisko/a155378.pdf) and the first attempt of such kind. Unfortunately, it's all described in rather informal pseudocode, a decision that led to a number of misunderstandings in follow-up criticism. Lenat has responded to that in one of the better known publications, [Why AM and EURISKO appear to work](/docs/am-eurisko/Why_AM_and_EURISKO_Appear_to_Work.pdf).

AM was built around concept formation process utilizing a set of pre-defined heuristics. EURISKO takes it a step further, adding the mechanism of running discovery search on its own heuristics. Both are specimen of what we could call 'Lisp-complete' programs: designs that require Lisp or its hypothetical, similarly metacircular equivalent to function. Their style was idiomatic to INTERLISP of 1970s, making heavy use of FEXPRs and self-modification of code.

There's quite a lot of thorough analysis available in three-part `The Nature of Heuristics`: [part one](/docs/am-eurisko/Heuristics_I.pdf), [part two](/docs/am-eurisko/Heuristics_II.pdf).  [The third part](/docs/am-eurisko/Eurisko_Heuristics_III.pdf) contains the most insights into the workings of EURISKO. Remarkable quote of when EURISKO discovered Lisp atoms, reflecting it was written before the two decade pause in nuclear annihilation threat:

`Next, EURISKO analyzed the differences between EQ and EQUAL. Specifically, it defined the set of structures which can be EQUAL but not EQ, and then defined the complement of that set. This turned out to be the concept we refer to as LISP atoms. In analogy to humankind, once EURISKO discovered atoms it was able to destroy its environment (by clobbering CDR of atoms), and once that capability existed it was hard to prevent it from happening.`

Lenat's [eventual conclusion](/docs/am-eurisko/On_the_thresholds_of_knowledge.pdf) from all this was that "common sense" is necessary to drive autonomous heuristic search, and that a critical mass of knowledge is necessary. That's where his current CYC project started off in early 1990s.

Bonus material: `The Elements of Artificial Intelligence Using Common Lisp` by Steven L. Tanimoto describes a basic AM clone, [Pythagoras](http://web.cecs.pdx.edu/~mperkows/CLASS_ROBOTICS/LISP/tanimoto/PYTHAG.CL). 
