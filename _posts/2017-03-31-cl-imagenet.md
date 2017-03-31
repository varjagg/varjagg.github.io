---
layout: post
title: Sifting through ImageNet dataset in Lisp
category: Lisp
---

..there is no shortage of rainy evenings in the rain capital of the world, so I used a few of them to put together this small application that I called (perhaps overly ambitiously) [cl-imagenet](https://github.com/varjagg/cl-imagenet). It uses a bunch of Lisp libraries: [opticl](https://github.com/slyrus/opticl) and [cl-jpeg](https://github.com/varjagg/cl-jpeg) for image processing, [cxml](http://common-lisp.net/project/cxml/) for extracting bounding boxes from the annotations, [cl-fad](http://weitz.de/cl-fad/) for filesystem operations, and [trivial-channels](https://github.com/rpav/trivial-channels) in combination with [clx](https://github.com/sharplispers/clx) for streaming to display.

<iframe width="560" height="315" src="https://www.youtube.com/embed/m5qrVH-7WpM" frameborder="0" allowfullscreen></iframe>

The code tries to detect how many cores the host machine has, then creates the corresponding number of worker units. The workset ImageNet subunits list is built up, which are then assigned to the workunits. Each workunit fetches annotation file, extracts the bounding boxes and image file reference, decodes the corresponding JPEG file, handles processing with OptiCL and sends the result via shared channel to display thread. It is impressive compact the code is when leveraging random bits of the ecosystem available through Quicklisp.

In this setup only the luminance component of JPEG is extracted and then thresholded from medium gray. The video is filmed on an old quad i5-2500. On my 8-core i7-6700 box with visualisation off, it averages some 200K processed images per hour.

Tested lightly with SBCL on Linux. One known problem place is the message channel gradually eating up memory with visualization on, but as it's only used in tests it hasn't been a pressing need to fix yet.
