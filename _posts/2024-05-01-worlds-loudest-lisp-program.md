---
layout: post
title: The World's Loudest Lisp Program
category: Lisp Psychoacoustics
---

It is interesting that while I think of myself as a generalist developer the vast portion of my career has been towards embedded and systems programming. I'm firmly a Common Lisp guy at heart but embedded tech landscape is entrenched realm of C sprinkled with some C++ and nowadays Rust. However I had incredible fortune to work for the last few years on a substantial embedded system project in Common Lisp.

The story starts in Western Norway, the world capital of tunnels with over 650 located in the area. Tunnels are equipped and maintained to high standard and accidents are infrequent but by the nature of quantities serious ones get to happen. The worst of these are naturally fires, which are notoriously dangerous. Consider that many of single bore tunnels have length over 5km (and up to 24km). Some of them are undersea tunnels in the fjords with inclination of up to 10 degrees. There are no automatic firefighting facilities. These are costly both in installation and maintenance, and while they might work in a country with one or two tunnels total they simply do not scale up. Hence the policy follows the self evacuation principle: you're on your own to help yourself and others to egress, hopefully managing to follow the signage and lights before the smoke sets in and pray the extractor fans do their job.

![Aftermath of a fire](/images/loudest-lisp-program/gudvangatunnelen.jpg)

So far Norway have been spared of mass casualty tunnel fires but there have been multiple close calls. One of particularly unlucky ones, the 11.5km long Gudvangatunnelen had experienced fires in span of a few years. Thus national Road Administration put forth a challenge to develop a system to augment self-assisted evacuation. [Norphonic](https://norphonic.com/), my employer, had won in a competition of nine contenders on the merits of our pre-existing R&D work. In late 2019 the project has officially started, and despite the setbacks of the pandemic concluded in 2021 with series production of the system now known as [Evacsound](https://evacsound.com/). The whole development on this project was done by a lean team of:

* software engineer who could also do some mechanical design and basic electronics
* electrical engineer who could also code
* two project engineers, dealing with product feasibility w.r.t. regulation and practices, taking care of SCADA integration and countless practicalities of automation systems for tunnels
* project coordinator who communicated the changes, requirements and arranged tests with the Road Administration and our subcontractors
* logistics specialist ensuring the flow of hundreds of shipments back and forth on the peak of pandemic

![Live hacking](/images/loudest-lisp-program/wrp_node.jpeg)
*Wesley, our EE patching up a prototype live*

Atop of this we were also hiring some brilliant MEs and EEs as contractors. In addition two Norway's leading research institutes handled the science of validating psychoacoustics and simulating fire detection.

At this point the system is already installed or is being installed in 6 tunnels in Norway with another 8 tunnels to some 29km total on order. We certainly do need to step up our international marketing efforts though.

![In the tunnels](/images/loudest-lisp-program/evac_whizz.gif)

# The Concept

How do you approach a problem like this? The only thing that can be improved under self-evacuation is the flow of information towards people in emergency. This leaves us with eyesight and hearing to work with. Visual aids are greatly more flexible and easy to work with. However their huge drawback is their usefulness expires quickly once the smoke sets in.

Sound is more persistent, although there are numerous challenges to using it in the tunnels:

* The background noise from smoke extraction fans can be very high, and if you go for speech the threshold for intelligibility has to be at least 10dB over the noise floor
* Public announcement messages alone are not very efficient. They are great in the early phase of fire to give heads up to evacuate, but kind of useless once the visibility is limited. At that point you also know you are in trouble already.
* Speech announcements rely on comprehension of the language. In one of Gudvangatunnelen fires a bus full of foreign tourists who spoke neither Norwegian nor English has been caught in the thick of it. Fortunately a local lorry driver stopped by to collect them.
* Acoustic environment in tunnels ranges from poor to terrible. Echo of 4-5 seconds in mid-range frequencies is rather typical. 

In addition to above, the system should have still provided visual clues and allow for distributed temperature sensing for fire detection. It has also to withstand pressure wash along the tunnel wall, necessitating IP69 approval.

We decided to start our design from psychoacoustics end and let the dice fall for the rest. The primary idea was to evacuate people by aiding with directional sound signals. The mechanism was worked out together with SINTEF research institute who conducted live trials on general population. A combination of sound effect distance requirements and technical restrictions in the tunnel has led us to devices installed at 3m height along the wall at 25m intervals. Which was just as well, since it allowed both for application of acoustic energy in lest wasteful, reverberating way *and* provided sensible intervals for radiated heat detection.

![Node dissected](/images/loudest-lisp-program/node_section.png)

A typical installation is a few dozen to several hundred nodes in a single tunnel. Which brings us to the headline: we have projects that easily amount to tens of kilowatts acoustic power in operation, all orchestrated by Lisp code.

# Tech stack

The hardware took nearly 20 design iterations until we reached what I would immodestly call the Platonic design for the problem. We were fortunate to have both mechanical and electronic design expertise from our [other products](https://norphonic.com/products/voip-phones-and-accessories/). That allowed us to iterate at an incredible pace. Our software stack has settled on Yocto Linux and Common Lisp. Why CL? That's what I started our earliest design studies with initially. Deadlines were tight, requirements were fluid, the team was small and I can move in Common Lisp really, really fast. I like to think that am also a competent C programmer but it was clear doing it in C would be many times the effort. And with native compilation there's no performance handicap to speak of, so it is hard to justify a rewrite later.

![Design iterations](/images/loudest-lisp-program/iterations.jpg)

Our primary CL implementation is Lispworks. There are some practical reasons for that.

* Its tree shaker is really good. This allows our binaries to run on a system with 128 Mb RAM with room to spare, which at the scale of thousands devices manufactured helps keep the costs down.
* It officially supports ARM32 with POSIX threads, something only it and CCL did at the time.
* The garbage collector is [very tunable](https://www.lispworks.com/documentation/lw80/lw/lw-garbage-collection-ug.htm).
* There is commercial support available with implementors within the earshot. Not that we ended up using it much but the thought is soothing.

We however do use CCL liberally in development and we employ SBCL/x86 in the tests matrix. Testing across the three implementations has found a few quirks on occasions.

# System Design

At its heart Evacsound is a soft real time, distributed system where a central stages time synchronized operation across hundreds of nodes. Its problem domain and operational circumstances add some constraints:

1. The system shares comms infrastructure with other industrial equipment even though on own VLAN. Network virtualization abstraction breaks down in real time operation: the product has to tolerate load spikes and service degradation caused by other equipment yet be mindful of network traffic it generates.
2. The operations are completely unmanned. There are no SREs; nobody's on pager duty for the system. After commissioning there's typically no network access for vendors to the site anyway. The thing have to sit there on its own and quietly do its job for the next couple decades until the scheduled tunnel renovation.
3. We have experience designing no-nonsense hardware that lasts: this is how we have repeat business with Siemens, GE and other big players. But with sheer scale of installation you can count on devices going dark over the years. There will be hardware faults, accidents and possible battle attrition from fires. Evacsound has to remain operational despite the damage, allow for redundant centrals and ensure zero configuration maintenance/replacement of the nodes.

The first point has channeled us to using pre-uploaded audio rather than live streaming. This uses the network much more efficiently and helps to eliminate most synchronization issues. Remember that sound has to be timed accounting for propagation distances between the nodes, and 10 millisecond jitter gives you over 3 meters deviation. This may sound acceptable but a STIPA measurement will have no mercy. Then, the command and control structure should be flexible enough for executing elaborate plans involving sound and lighting effects yet tolerate inevitable misfortunes of real life.

The system makes heavy use of CLOS with a smattering of macros in places where it makes a difference. Naturally there's a lot of moving parts in the product. We're not going into the details of SCADA interfacing, power and resource scheduling, fire detection, self calibration and node replacement subsystems. The system has also distinct PA mode and two way speech communication using a node as a giant speakerphone: these two also add a bit of complexity. Instead we're going to have an overview on the bits that make reliable distributed operation possible.

![Test of fire detection](/images/loudest-lisp-program/fire_test.gif)

## Processes

First step in establishing reliability baseline was to come up with abstraction for isolated tasks to be used both on the central and on the nodes. We built it on top of a thread pool, layering on top of it an execution abstraction with start, stop and fault handlers. These tie in to a watchdog monitor process with straightforward decision logic. An Evacsound entity would run a service registry where a service instance would look along these lines:

{% highlight lisp %}
(register-service site
		  (make-instance 'avc-process :service-tag :avc
				 :closure 'avc-execution
				 :suspend-action 'avc-suspend
				 :resume-action 'avc-resume
				 :process-name "Automatic Volume Control"))
{% endhighlight %}

…and the methods that would be able to spin, quit, pause or resume the process based on its `service-tag`. This helps us ensure that we don't ever end up with a backtrace or with an essential process quietly knocked out.

## Plans

To perform its function Evacsound should be able to centrally plan and distributed execute elaborate tasks. People often argue what a DSL really is (and does it really have to have macros) but in our book if it's special purpose, composable and is abstracted from implementation details it is one. Our planner is one example. We can create time distributed plans in abstract, we can actualize abstract plans with specific base time for operations, we can segment/concatenate/re-normalize plans in various ways. For instance, below is a glimpse of abstract plan for evocation generated by the system:

{% highlight lisp %}
(plan-modulo
 (normalize-plan
  (append (generate-plan (left accident-node)
			 :selector #'select-plain-nodes
			 :time-shift shift
			 :direction :left
			 :orientation :opposite)
 	  (generate-plan (right accident-node)
			 :selector #'select-plain-nodes
			 :time-shift shift
			 :direction :right
			 :orientation :opposite)))
 (* 10 +evacuation-effect-duration+))
{% endhighlight %}

We can see above that two plans for each evacuation direction are concatenated then re-normalized in time. The resulting plan is then modulo adjusted in time to run in parallel subdivisions of specified duration.

Generated plans are sets of node ID, effect direction and time delta tuples. They do not have association of commands and absolute times yet, which are the job of `ACTUALIZE-PLAN`.

## Command Language

The central and nodes communicate in terms of CLOS instances of the classes comprising the command language. In simplest cases they have just the slots to pass values on for the commands to be executed immediately. However with appropriate mixin they can inherit the properties necessary for precision timing control, allowing the commands to be executed in time synchronized manner across sets of nodes in plans.

It is established wisdom now that multiple inheritance is an anti-pattern, not worth the headache in the long run. However Evacsound make extensive use of it and over the years it worked out just fine. I'm not quite sure what the mechanism is that makes it click. Whether it's because CLOS doesn't suffer from diamond problem, or because typical treatment of the objects using multiple dispatch methods, or something else it really is a non-issue and is a much better abstraction mechanism than containment.

## Communication

The next essential task is communication. Depending on the plan we may communicate with all or subsets of nodes, in particular sequence or simultaneously, synchronously or async, with or without expectation of reported results. For instance we may want to get a noise estimation from microphones for volume control, and that would need to be done for all nodes at once while expecting a result set or reports. A PA message would have to be played synchronized but the result does not really matter. Or a temperature change notice may arrive unprompted to be considered by fire detection algorithm.

This particular diverse but restricted set of patterns wasn't particularly well treated by existing frameworks and libraries, so we rolled our own on top of socket library, POSIX threads and condition variables. Our small DSL has two basic constructs, the asynchronous `communicate>` for outgoing commands and `communicate<` for expecting the result set, which can be composed as one operation `communicate`. A system can generate distributed command such as

{% highlight lisp %}
(communicate (actualize-plan
	      (evacuation-prelude-plan s)
	      'fuse-media-file
	      (:base-time (+ (get-nanosecond-time) #.(2ns 1.8)))
	      :sample-rate 32000
	      :media-name "prelude"))
{% endhighlight %}

What happens here is that previously generated plan is actualized with `FUSE-MEDIA-FILE` command for every entry. That command inherits several timing properties:

* absolute `BASE-TIME` set here explicitly
* `DELTA` offset which is set from the plan's pre-calculated time deltas
* `TIME-TO-COMPLETE` (implicit here) which specifies expected command duration and is used to calculate composite timeout value for `COMMUNICATE`

If any network failure occurs, a reply from the node times out or node reports a malfunction an according condition is signaled. This mechanism allows us to effectively partition distributed networked operation failures into cases conveniently guarded by HANDLER-BIND wrappers. For instance, a macro that just logs the faults and continues the operation can be defined simply as:

{% highlight lisp %}
(defmacro with-guarded-distributed-operation (&body body)
  `(handler-bind ((distributed-operation-failure
		   #'(lambda (c)
		       (log-info "Distibuted opearation issue with condition ~a on ~d node~:p"
				 (condition-name c) (failure-count c))
		       (invoke-restart 'communicate-recover)))
		  (edge-offline
		   #'(lambda (c)
		       (log-info "Failed to command node ~a" (uid c))
		       (invoke-restart 'communicate-send-recover))))
     ,@body))
{% endhighlight %}

This wrapper would guard both send and receive communication errors, using the restarts to proceed once the event is logged.

So the bird's eye view is,

* we generate the plans using comprehensible, composable, pragmatic constructs
* we communicate in terms of objects naturally mapped from the problem domain
* the communication is abstracted away into pseudo-transactional sets of distributed operations with error handling

Altogether it combines into a robust distributed system that is able to thrive in the wild of industrial automation jungle.

*TL;DR Helping people escape tunnel fires with Lisp and funny sounds*
