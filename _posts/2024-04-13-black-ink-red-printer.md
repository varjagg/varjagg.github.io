---
layout: post
title: Elektrnonika MS-6312 Inkjet Printer
category: Vintage Hardware
---

This printer was clearly "inspired" by Kodak Diconix-150. Metricised Centronics port, 160cps and graphics on A4 sheets, drum fed perforated and non-perforated paper. The controller board is built around KR580VM80A, an 8080 clone. This model appeared in USSR in 1989 and a few years later it was my first (and to this date the only) inkjet printer.

![MS6312](/images/black-ink-red-printer/ms6312.jpg)

I used this puppy, hooked into my Spectrum, in my freshman year to print semester assignments. My friend had coded a text editor and I wrote the printer driver. It worked OK, maybe with occasional slip in paper feed and head leaking the ink if left unused for long. Then one day it just died on me, and it went to storage.

Now I finally got around to examining it, as seen in the video below. There were three immediate issues:

1. The -5V rail for the CPU was down to -3.4V
2. 8 volt rail trace for steppers on flex cable was torn
3. About half of printing head flexicable traces have dissolved from the ink over the decades

<iframe width="560" height="315" src="https://www.youtube.com/embed/dR0hY7Kx9NA?si=Gad42fDSdXRtwOK-" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

Printing head cable is entirely passive output so couldn't be affecting controller operation, while both (1) and (2) are clearly the issues. The trace on the cable was patched up. Looking at the power circuit schematics VD8 Zener appears to be potential culprit. So it was promptly replaced with a Western 5V substitute and the rail went down to -4.6V. Still out of nominal 5% tolerance of the CPU but we'll see if that's enough to kick it into life.

![PSU schematics](/docs/soviet_inkjet/schematics2.gif)

_To be continued…_
