---
title: Rationale - Why Kuljet?
---

The first objective is to explore the space of small languages dedicated to
a particular task.

Kuljet has special syntax for declaring HTTP endpoints, interacting with
the database, and building HTML -- all in a type safe manner requiring
few type annotations.

The second objective is to break down the barriers between typed,
functional languages and the relational model.

In many typed languages with database integration, you are required to
annotate the type of relational operations and use an embedded DSL for
writing SQL expressions. Kuljet has no problem inferring the type of
natural joins, selections or projections. SQL expressions can be
written using the same syntax as the rest of your code i.e. you can
use standard operators and even call functions.

For example, with `table example { x : int }`:

```kuljet
let add1 = fun x -> x + 1 in
example select { y = add1 x }
```

is a valid Kuljet projection of type `query { y : int }` and compiles
to SQL code like:

```sql
SELECT x + 1 AS y FROM example
```
