---
layout: post
title: Announcing CL-PJSIP
category: Lisp
---

I am pleased to announce [CL-PJSIP](https://github.com/varjagg/cl-pjsip), a Common Lisp wrapper for [PJSIP](http://pjsip.org/), a popular multimedia communications library.

CL-PJSIP so far supports a limited subset of PJSIP functionality, yet sufficient to implement a simple SIP telephony user agent. Things will get gradually expanded from that.  At the moment, focus is on moving beyond alpha-quality (scary amounts of FFI there) and implimenting Lisp-ideomatic handling of PJSIP components.

There is a certain learning curve involved in using PJSIP, and it's worth starting with the included `cl-pjsip-ua.lisp`. This is a near verbatim copy of PJSIP's own simpleua.c.

The application runs here (disclaimers apply) on CCL 1.11 LinuxX8664, although it does seem to run on recent SBCL (albeit not really tested). There are couple +DARWINs in the code, where it was inferred from PJSIP header files, but it was not tested on MacOS at at all.

