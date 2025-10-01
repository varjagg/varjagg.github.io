---
layout: post
title: Vibecoding a 3D GUI for System Commissioning
category: LLM
permalink: /3d-commissioning-gui/
---
I'm really not a frontend guy. I know a number of superb frontend developers and admire their work but the ecosystem, the churn, and the focus around human end user experience are all positively intimidating. So ideally for an occasional UI project we would just hire consultants. Sometimes the stars alinged so I was pressed to do Web UIs but they all were minor specialty projects.

Our latest product is [Evacsound](https://blog.funcall.org/lisp%20psychoacoustics/2024/05/01/worlds-loudest-lisp-program/) where am pretty proud of programming I've done. However there isn't much in terms of UI: such products customarily integrate into customer's existing SCADA (Supervisory Control and Data Acquisition) systems. These usually are built using specialist solutions with decades of history. They are informative and robust but not necessarily very ergonomic or visually exciting. Either way they are very Not Our Problem area here at Norphonic.

However no product is truly complete until it's free from developer intervention in its entire life cycle. For Evacsound the remaining dirty spot was project commissioning. Tunnels have different layouts and even topology: two of our latest deliveries have 6 underground roundabouts between them. Technical rooms, escape tunnels and ramps are also common. So you can't really provide these layouts for customer projects out of the box. This phase so far was done by us using some auxiliary programs and SQL queries. They did not abstract internal layout representation sufficiently so an outsider could use it. What we needed was an end-user commissioning tool to allow site definition by unaffiliated automation engineers. That would make things simpler (and cheaper!) for everyone involved. A vendor-independent commissioning procedure also greatly simplifies marketing the system internationally.

So you see where it's going. As the design was depending on minute knowledge of Evacsound operating principles, onboarding a consultant would have taken quite a bit of time, and we'd still have to follow up tightly in development. Hence this became the first production project where I decided to give agents a shot, not least inspired by [this tptacek post](https://fly.io/blog/youre-all-nuts/). My rough estimate for completing this project by myself was maybe 4 months, including 6-7 weeks for learning Javascript, three.js and up to date frontend practices. Using Codex I completed it in 5 weeks worth of intermittent dev time between other projects (and saved myself from learning JS). I believe an experienced front-end developer could complete this AI aided somewhat sooner. However substantial part of work was exploring the conceptual UX space and adopting backend (also by me, manually) for the best fit: this can't be readily optimized away.

As this was a greenfield internal project there was no customer with strong aesthetic preferences to contend with. Tunnel designs by nature are very spatial so I leaned into late century three dimensional naïve wire-frame look. No hidden line elimination here is both an aesthetic choice and a visual aid to make sense of scene contents. With color palette it was settled on bold green for structure and controls, red for selections and yellow for auxiliary elements. Neon blue is reserved for elements under automatic detection and distance calibration but this flow is not represented in this demo.

The resulting code as far as I can judge is mediocre but not terrible. There is a panel of tests for most of functionality and a number of generated documents. The demo you can see below, or as a stand-alone version [here](). Use mouse to orbit and select elements. The elements can be added on zigzag spacers using the buttons in the upper row or by pressing Enter if you want a node. They can be deleted with Del or Backspace.

<iframe src="{{ '/assets/evedemo/index.html' | relative_url }}"
        style="width:100%;height:800px;border:1px solid #ccc;">
</iframe>

So this was an interesting experience. There's some debate how true the metrics are but I honestly think the AI allowed me to move much faster than I could otherwise. What does that spell for our trade, job prospects, junior pipeline and enjoyment of our craft is perhaps best left for a separate post. For now let me summarize my practical AI process experience thus far into Eugene's Eightfold Path:

1. Use an agent. You're not going to build anything substantial by copying and pasting code to/from the chat window. Before you say 'oh but it works for me only wish the AI wasn't this dumb' no just shut up and try a coding agent. There are local agents around and there are offerings by the big companies. Any of them will be better than copypasting simply because they provide the embodiment for code within the system/toolchain which helps eliminate hallucination issues.

2. Have some programming experience. Contrary to many enthusiastic reports you totally need it. Perhaps that would change one day but at the moment I can't see how. To take advantage of LLMs in a problem domain you need to have substantial expertise in the domain, that's just how it works. You should know what is simple and what is hard to do: this gives you a better chance formulating the requests in terms of what's possible. You want to recognize when the agent misunderstood you or made a poor design choice. You need some sense of taste to code as ugly solutions give the model gradient descent into intractable, unfixable mess down the road.

3. It helps if you're a half decent writer. Frankly a double major in English (or any other language) lit & CS could be the best skill set for leveraging this technology. LLM appreciates focused description, metaphors, relevant detail and precision use of adjectives. All this helps to map your wants into its thousands-dimensional conceptual space. Anxious about that great novel you never finished or even started? Well here's the place to flex your literary muscle.

4. Perform one simple request at a time. Do not combine even related tasks; don't submit multi-step stacks of work in one prompt. On each your request LLM essentially runs an optimization job, and multi-parameter optimizations are hard to do well. Sequence everything into separate requests. Chew each one down with the best description you can come up.

5. Steer the agent with strategic design choices. This is directly related to the second point. If you know what an internal data structure or algorithm would be a perfect match for the problem start with that. By nature of iterative development the agent will come up with the simplest structure to solve the immediate request. Long term it would become outdated and what the agent is likely to do is to put more graft and patches atop of that. Since you presumably have a further horizon insist on sensible design choices from the start. You should help solidify the foundation so that LLM has easier time putting meat on the bones.

6. Separate functionality into different files each few hundred lines long. How long is too long will change as available compute grows but that's the idea. It simplifies reasoning for the model by reducing the context.

7. Add basic documentation and make the agent follow it up and update as necessary. I went for a bird eye view design document plus specific documents for each subsystem. It has a two-fold benefit of helping keep the LLM grounded and providing readable summary diffs for you after each iteration.

8. Use the technology you understand for incomprehensible things will hamper your participation. In my case it was eschewing fancy frameworks for plainest JS possible.
