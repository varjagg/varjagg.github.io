---
layout: post
title: A tiny Lisp bytecode interpreter in Z-80 assembly
category: Lisp
---

It all started with a raid on a long abandoned hosting service. Seen a mention of it in the news, leading to a vague recollection of using it for something. Email address associated with the account was long defunct, and the service itself changed ownership a few times in the past two decades. But incredibly, I could recall login credentials *and* they worked still.

Amazingly, in a pile of abandoned HTML templates, obsolete software archives and Under Construction GIFs there was a source file for a project I long considered lost. It's a minimal [Lisp bytecode interpreter](https://github.com/varjagg/zxlispvm) written in assembly for ZX Spectrum along the lines of MIT [AIM-514](https://dspace.mit.edu/handle/1721.1/5731). Save for address locations and maybe a couple ROM calls for erorr reporting it's generic Z-80 code.

It was a part of bigger project that should have included a primitive REPL, but no trace of that was found. Also, am quite sure there is a henious bug lurking in the mark&speep GC. Should really find time to finally debug that!
