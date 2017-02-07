---
layout: post
title: Announcing CL-VIDEO
category: Lisp
---

I'm happy to announce [CL-VIDEO](https://github.com/varjagg/cl-video), a basic AVI/MJPEG video decoder written in Common Lisp. The library leverages [CL-JPEG](https://github.com/sharplispers/cl-jpeg) for frame processing and [CL-RIFF](https://github.com/RobBlackwell/cl-riff) for container format handling.

The code has only been lightly tested on SBCL 13.x/Linux x86-64. Some sample files can be found [here](https://cinelerra-cv.org/footage.php) (the toy plane AVI) and [here](http://jjc.freeshell.org/turning_pages.html).

To run it, CL-JPEG version 2.7 is required. Since it's not in Quicklisp yet as of time of writing, it must be cloned into your `local-projects`. Then you can load `cl-video-player` system and run `(cl-video::play <your filename.avi>)`.

The implementation is still very much naive, as expected from a weekend project. There is no support for indexing, and a number of edge cases of legit AVI encodings might fail. The library will decode video frames in the order they occur. No parsing of `BITMAPINFOHEADER` structures; the assumption is they are MJPG DIB. No audio playback, although the stream is being read and adding at least PCM/WAV playback shouldn't be too big an effort.The decoder is factored out into independent implementation in `cl-video.lisp`. A primitive CLX media player is included in `player.lisp`. Each AVI instance contains one or more (audio or video) stream record objects, which hold ring buffers of frames with read and write cursors. The interaction between the decoder and player frontend runnning in separate threads is done via mutexed access to this ring buffer. The player thread can be kicked off a callback supplied to decode method: it will be called once the header part is parsed.

Since CL-JPEG doesn't use any SIMD extensions, the performance is modest. On my 6 year old 3GHz i5 (running one core of course) it decodes 480p 25fps file without hiccups, but going beyond that would require implementing multicore support in decode. Still it might be useful as is in applications not requiring real-time decoding performance.
