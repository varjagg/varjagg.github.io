---
layout: post
title: About Time
category: Lisp
---

This week I put together a [small NTP client](https://github.com/varjagg/cl-ntp-client). To keep dependencies at minimum and to avoid forcing a permanently running process onto users, it does not attempt to adjust system RTC clock, compensate jitter or evaluate time server quality. As I see it, much of that behaviour is easy enough to add via mixins with the defined `NTP` class.

NTP timestamp is two 32-bit values: seconds and fraction of a second. NTP conveniently counts seconds from Jan 1 1900, just like universal time in Common Lisp. There is however no portable Common Lisp representation for fractions of a second. Thus the client sticks to using NTP formatted fraction for that. It is way more precision than any existing CL implementation has in `INTERNAL-TIME-UNITS-PER-SECOND`, but this makes this value comparable across implemenations. The new `GET-ADJUSTED-UNIVERSAL-TIME` method then returns a pair of values: universal time and NTP fraction. The fraction can be converted to the implementation's internal time scale with `FRACTION-TO-INTERNAL`.

Internally we define no special arithmetic on NTP timestamps but provide two conversion functions for single integer space. `BIG-TIME` converts NTP stamp into a large integer. We then do all calculations in that domain, and convert back to NTP timestamp using `SMALL-TIME` when it's time to send it over the wire. An `NTP` instance stores adjusted time as an offset from internal real time. The offset is roughly intialized with universal time and then adjusted after each server request.
