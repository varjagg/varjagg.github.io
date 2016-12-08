---
layout: post
title: PJSUA support in CL-PJSIP
category: Lisp
---

After a bit of tweaking, [CL-PJSIP](https://github.com/varjagg/cl-pjsip) now supports basic [PJSUA API](http://www.pjsip.org/pjsip/docs/html/group__PJSUA__LIB.htm). PJSUA aggregates much of PJSIP functionality in a handful of structures and protocol methods. This simplifies application side a lot: one can get by with just a few lines of setup code and a couple callbacks. From Lisp perspective it also reduces the used FFI surface to a stable, generic interface. This ought to improve long term compatibility with PJSIP own revisions.

I refer to `cl-pjsua-demo.lisp` in the library for a short sample. Try `(cl-pjsip::run-pjsua "sip:411@ideasip.com")` for a quick test against a voice menu directory. It was tested on Linux x86_64 with CCL 11.1 and Allegro 10.1 beta. It however eventually crashes with floating point exception on SBCL. 
