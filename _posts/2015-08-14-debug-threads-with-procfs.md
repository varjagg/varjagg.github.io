---
layout: post
title: Wait channels as debugging aid for threading bugs
category: Debugging
---

..In every system programmer's career inevitably comes a moment when they encounter a non-obvious, asymptomatic, poorly reproduceable bug in critical software running on remote site without remote debug instrumentation.

Well there was a sympthom. The device would put up a diagnostic message amounting to "oops" and stop responding to stimuli. The log file would stop growing while the generating thread is still around. The process would not react to SIGQUIT (denying me a core dump). In other words, a very good hint of a deadlock.

To top that, the issue would appear only on a few devices out of a hundred, and after running for at least a few weeks.

In cases like this, however, there's still a great tool at disposal: `wchan` (wait channel) of procfs. Per the manpage:

```
 (35) wchan  %lu
      This  is the "channel" in which the process is waiting.  It is the address
	  of a location in the kernel where the process is sleeping.  The corresponding
	  symbolic name can be found in /proc/[pid]/wchan.
```

Each process has also associated tasks, in this case threads, found in `/proc/[pid]/tasks/`, which is again a list of PIDs with a `wchan` field associated. I start with skimming through the application threads on an affected system, looking for something out of ordinary.

The application is structured around a central SYSV message queue, where a state machine dispatches the messages and notifies the subscribed functional modules. Some of the modules are separate threads, but a number of simpler tasks are executed direcly in the dispatcher thread.

Looking at the dispatcher process first reveals it is blocked in `do_msgsnd()`: the kerenel call for sending SysV IPC message:

```
 # cat /proc/306/task/311/wchan 
 do_msgsnd
```

Now, sending a message can block in one case only: the queue is full. Again, we can check that via procfs.

```
 # cat /proc/sysvipc/msg
       key      msqid perms      cbytes       qnum lspid lrpid 
    151273          0     0       16384        128   306   306
```

Yep, `qnum` is at 128, the limit on the system, so the queue is full. The only place that consumes the queue is the dispatcher thread, and the most likely way for *that* part to lock up is for notification loop to block on some call.

After checking the `wchan` value of the dispatcher thread versus one on a healthy system, the likely offender is found:

```
 # cat /proc/306/task/317/wchan 
 unix_wait_for_peer
```

This is the kernel call handling the opening of Unix socket. The only place where communication over `AF_UNIX` happens is the code forwarding system status over to snmpd. Upon examination it turns out *cough* the socket is being opened and closed for every message session. Normally this shouldn't be a problem, but the system runs with pretty ancient kernel, affected by cetrain bugs like [CVE-2009-3621](http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2009-3621), and this is just not a good practice in general.

The conclusion is, there is a way out of most desperate situations. If you think you are stuck, remember it's nothing compared to what Apollo 13 crew had to go through.



