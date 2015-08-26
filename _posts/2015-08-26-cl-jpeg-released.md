---
layout: post
title: CL-JPEG has been updated
category: Lisp
---

A long due update to Common Lisp JPEG Library has now been merged into [sharplispers/cl-jpeg](https://github.com/sharplispers/cl-jpeg).

The summary of changes:

* The various global state tables and counters were moved into special variables of decoder/encoder functions. This should address the concerns of thread safety.
* The monolithic source file was broken up into several according with modern way of structuring the projects.
* Metadata for :author and :description was added to the project's .asd.
* The contributors are now listed. Do let me know if I forgot to add someone.

Revisiting own 16 year old code was an interesting experience. Thanks to everyone who kept the project alive over the years.
