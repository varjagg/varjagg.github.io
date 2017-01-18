---
layout: post
title: New in CL-JPEG
category: Lisp
---

In course of last few months there were numerous small changes introduced to [CL-JPEG](https://github.com/varjagg/cl-jpeg). None substantial enough to warrant own announcement, but taken together perhaps it's due for an update. So here we go, as of version 2.6:

* Pre-allocated buffers in `DECODE-IMAGE` and `DECODE-STREAM` are now supported. This should help reduce consing in bulk-processing applications. The buffer can be pre-allocated based on dimensions via `JPEG:ALLOCATE-BUFFER`.
* CMYK JPEG support. YCCK to CMYK conversion is now performed by decoder. To convert into 3-component RGB, use `JPEG:CONVERT-CMYK-TO-RGB` convenience function.
* An option to disable colorspace conversion via `:COLORSPACE-CONVERSION NIL` supplied to decode functions has been added. Can be useful e.g. if one needs the luminance component. Support of the corresponding option for encoder to improve performance in transcoding applications is in the plans.
* Small bugfixes and performance tweaks.
