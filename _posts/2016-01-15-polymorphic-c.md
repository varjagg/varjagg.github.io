---
layout: post
title: Polymorphism with clay and sticks
category: C
---

Sometimes in discussions of obsolete C features, `union` comes up. Modern style guides and books view it as poor man's `struct` used in the old days to shave off a few bytes. Ben Klemens in his _21st_Century_C_ makes it an argument that in near all uses `union` makes code more error prone.

Here's however an example where unions might come handy.

Imagine you have an application managing several list structures which are similar but not quite the same. They all have list links and character names, but the payloads are quite dissimilar. One obvious way is to reference type-specific payload from the list node. However implementing this naïvely leads to an extra level of indirection in manipulating code.

Let's say we want to have data types for event subscriber mechanism and a FSM. 

{% highlight c %}

typedef struct subscriber {
  struct subscriber *next;
  module_rec *mod;
} subscriber;

typedef struct event {
  struct event *next;
  char *name;
  subscriber *subscribers;
} event;

typedef struct transition {
  struct transition *next;
  char *name;
  struct state *node;
} transition;

typedef struct state {
  struct state *next;
  char *name;
  transition *trans;
} state;

{% endhighlight %}

The common theme here is the list link, followed by name and then a pointer to payload structure.

Below is the necessary indirection definitions tying it all together into one, unobjectionable to the compiler, data type.

{% highlight c %}

typedef struct node {
  union entry *next;
  char *name;
} NODE;

typedef union entry {
  NODE link;
  state s;
  event e;
  transition t;
} entry;

{% endhighlight %}

Notice how `node` contains the character `name`, shared by all. In a way it is a "superclass" structure to all of the list types. We then can implement entirely type agnostic lookup and traversal methods.

{% highlight c %}

entry* create_entry(char *name, entry *next)
{
  entry* result;

  if((result = malloc(sizeof(entry)))) {
    result->link.name = name;
	result->link.next = next;
    return result;
  }
  return NULL;
}

entry* lookup_entry(char *name, entry *cur)
{
  
  while(cur && strcmp(cur->link.name, name)) {
    cur = cur->link.next;
  }

  return cur;
}
{% endhighlight %}

The code remains generic. Moreover, due to the constant `union` container size, we can share the memory allocation code. We can refer the types natively further in the code, save perhaps for a cast to/from the generic `entry` values when using the methods:

{% highlight c %}

    event* e = (event*)lookup_entry(msg.payload.name, (entry*)events);
      if(e) {
	   sub = e->subscribers;
	   ......
      }
	  
{% endhighlight %}

It might not seem like a big deal just for a couple of trivial access methods, but the same approach can be used to many other classic and esoteric container data types.
