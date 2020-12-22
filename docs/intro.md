---
title: Introduction
---

*Kuljet* is a happy little language for writing database-backed HTML applications.
Its type checking and integration with relational databases should help you whip up
prototypes in no time.

It aims to answer the question "What if a Haskeller designed PHP?" despite
nobody having asked it.

Features:

* Made for handling HTTP and building HTML.
* Seamless database queries - translates the syntax tree into SQL expressions.
* Type-checked relational operators (projection, selection, joins, etc.)
* Tasty curry flavoured functions.


## Hello World

```kuljet
serve get / = "Hello World!"
```

## Current State

*Kuljet* is still a work in progress. Expect it to change
dramatically.

It can be used for very simple HTML apps, but there will likely be a
lot of missing functionality and bugs.

Kuljet has a high potential for performance, but no effort has been
put towards this goal yet. Favourable benchmarks are coincidental.


## What Next?

Take a look at **the examples** such as [A Simple Chat Server](/examples/chat/).

If you'd like to start writing Kuljet, read the [installation instructions](/install/) and
start looking through [the guides](/guides/expressions/).
