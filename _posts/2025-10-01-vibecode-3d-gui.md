---
layout: post
title: Vibecoding a 3D GUI for System Commissioning
category: LLM
permalink: /3d-commissioning-gui/
---
First, I'm really not a frontend guy. Our latest product is [Evacsound](https://blog.funcall.org/lisp%20psychoacoustics/2024/05/01/worlds-loudest-lisp-program/) where am pretty proud of programming I've done. However there isn't much in terms of UI: such products customraily integrate into customer's existing SCADA (Supervisory Control and Data Acquisition) systems. These usually are built using specialist solutions with decades of history. They are informative and robust but not necessarily very ergonomic or visually exciting. Either way they are very Not Our Problem area here at Norphonic.

However no produt is truly complete until you don't need a developer intervention in its entire lifecylce. For Evacsound the remaining dirty spot was project commissioning. Tunnels have different layouts and even topologies: two of our latest deliveries have 7 underground roundabouts between them. Technical rooms, escape tunnels and ramps are also common. So you can't really provide these layouts for customer projects out of the box. This phase so far was done by us using some auxillary programs and SQL queries. They did not abstract internal layout representation sufficiently so it could be used by an outsider. What we needed was an end-user commissioning tool to allow site definition by unaffiliated automation engineers which would make things simpler (and cheaper!) to everyone involved. A vendor-independent commissioning procedure also greatly simplifies marketing the system globally.



As this was a greenfield internal project there was no customer with strong aesthetic preferences to contend with. I leaned into late century naïve wireframe look. No hidden line elimination here is both an aesthetic choice and a visual aid to make sense of scene contents. With color palette it was settled on bold green for structure and controls, red for selections and yellow for auxilliary elements. Neon blue is reserved for elements under automatic detection and distance calibration but this flow is not represented in this demo.

My rough estimate for completing this project by myself was 4-5 months, including 6-7 weeks for learning Javascript and up to date frontend practices. Using Codex I completed it in 6 weeks worth of intermittent dev time between other projects (and saved myself from learning JS). I believe an experienced front-end developer could complete this AI aided somewhat sooner. However substantial part of work was exploring the conceptual UX space and adopting backend (also by me, manually) for the best fit: this can't be readily optimized away.

The resulting code as far as I can judge is mediocre but not terrible. There is a panel of tests for most of functionality and a number of generated documents.

Below is a demo


1. Use an agent. You're not going to build anything substantial by copying and pasting code to/from the chat window. Before you say 'oh but it works for me only wish the AI wasn't this dumb' no just shut up and try a coding agent. There are local agents around and there are offerings by the big companies. Any of them will be better than copypasting simply because they provide the embodiment for code within the system/toolchain which helps eliminate hallucination issues.

2. Contrary to many enthusiastic reports you need some programming experience. Perhaps that would change one day but at the moment I can't see how. To take advantage of LLMs in a problem domain you need to have substantial expertise in the domain, that's just how it works. You should to know what is simple and what is hard to do: this gives you a better chance formulating the requests in terms of what's possible. You want to recognize when the agent misunderstood you or made a poor design choice. You need some sense of taste to code as ugly solutions give the model gradient descent into intractable, unfixable mess down the road.

3. It helps if you're a half decent writer. Frankly a double major at English lit & CS could be the best skillset for leveraging this technology. LLM appreciates focused description, metaphors, relevant detail and precision use of adjectives. All this helps greatly to map your wants into its thosuands-dimensional conceptual space. Anxious about that great novel you never finished? Well here's the place to flex your literary muscle.

4. Perform one simple request at a time. Do not combine even related tasks; don't submit multi-step stacks of work in one prompt. On each your request LLM essentially runs an optimization job, and multi-criteria optimizations are counter productive here. Sequence everything into separate requests. Chew each one down with the best description you can come up.

5. This is directly related to the second point: steer the agent with strategic design choices. If you know what an internal data structure or algorithm would be a perfect match for the problem start with that. By nature of iterative development the agent will come up with the simplest structure to solve the immediate request. Long term it would become outdated and what the agent is likely to do is to put more graft and patches atop of that. Since you presumably have a further horizon insisit on sensible design choices from the start. You should help solidify the foundation so that LLM has easier time putting meat on the bones.

6. Separate functionality into different files each few hundred lines long. It simplifies reasoning for the model by reducing the context.

7. Add basic documentation and make the agent follow it up and update as necessary. I went for a bird eye view design document plus specific documents for each subsystem. It has a two-fold benefit of helping keep the LLM grounded and providing readable summary diffs for you after each iteration.

8. Use the technology you understand for incomprehensible things will hamper your participation. In my case it was eschewing fancy frameworks for plainest JS possible.
