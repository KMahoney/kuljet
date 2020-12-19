---
title: The Structure of a Kuljet Program
---

A Kuljet program is made up of `table` declarations, `let` declarations and
`serve` declarations.

```kuljet
table |name| { |field|: |type|, ... }

let |name| = |expression|

serve |method| |path| = |expression|
```

## Table Declaration

A **table declaration** should reflect the names and
types of a table in your database. It has the form:

```kuljet
table |name| { |field|: |type|, ... }
```

e.g.

```kuljet
table people { name: text, age: int }
```

## Let Declaration

A top level **let declaration** binds a name to an expression
for all following declarations.

```kuljet
let |name| = |expression|
```

e.g.

```kuljet
let x = 1

let add1 = fun x -> x + 1
```

## Serve Declaration

A **serve declaration** routes a HTTP request to an
expression, and has the form:

```kuljet
serve |method| |path| = |expression|
```

`|method|` is the HTTP verb to match on (`get` or `post`).

`|path|` is a path pattern that can contain named variables.

`|expression|` is a Kuljet expression that results in a valid response.

e.g.

```kuljet
serve get /:name =
  p ["Hello ", name]

serve post /form =
  fun postVars: { name: text } ->
    p ["Hello ", postVars.name]
```

Those paying attention will have noticed that paths can
have placeholders starting with a colon (`:`) and `post` endpoints
can be defined as a function taking form-encoded inputs.

Most types can be inferred, but the type of the `postVars` argument
will need to be specified here.
