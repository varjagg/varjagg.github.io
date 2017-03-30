---
layout: post
title: Sifting through ImageNet in Lisp
category: Lisp
---

..there is no shortage of rainy evenings in Bergen, Norway, so I used a few of them to put together this small application that I called (perhaps overly ambitiously) [cl-imagenet](https://github.com/varjagg/cl-imagenet). It uses a bunch of Lisp libraries: [opticl](https://github.com/slyrus/opticl) and [cl-jpeg](https://github.com/varjagg/cl-jpeg) for image processing, [cxml](http://common-lisp.net/project/cxml/) for extracting bounding boxes from the annotations, [cl-fad](http://weitz.de/cl-fad/) for filesystem operations, and [trivial-channels](https://github.com/rpav/trivial-channels) in combination with [clx](https://github.com/sharplispers/clx) for streaming to display.

<iframe width="560" height="315" src="https://www.youtube.com/embed/m5qrVH-7WpM" frameborder="0" allowfullscreen></iframe>

The code tries to detect how many cores the host machine has, then creates the corresponding number of worker units. The workset ImageNet subunits list is built up, which are then assigned to the workunits. Each workunit fetches annotation file, extracts the bounding boxes and image file reference, decodes the corresponding JPEG file, handles processing with OptiCL and sends the result via shared channel to display thread.

Tested lightly with SBCL on Linux. One known problem place is the message channel gradually eating up memory when visualization is on, but as it's only used in tests it hasn't been a pressing need to fix yet.
