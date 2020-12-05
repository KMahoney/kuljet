---
title: Expressions
---

Those who have seen an ML-based language before will find Kuljet's
expression syntax familiar ground, and can probably skip to the
next section.

## Literals

Expressions can be literals, such as `"string"` or `123`.

Construct lists square brackets, like `[ 1, 2, 3 ]`.

Construct records with curly brackets, like `{ a = 1, b = 2 }`.

P.S. Access values of a record with a dot: `record.field`.

## Function Application

Unlike C-style languages, function application does not require
parentheses.  Given a function `a` which takes two integers and
returns an integer (i.e. type `int -> int -> int`), then `a 1 2` calls that
function with arguments `1` and `2`.

Functions are partially applied, so `a 1` gives a function of type
`int -> int`, and `(a 1) 2` is the same as `a 1 2`.

Use parentheses for nested application e.g. `a (a 1 2) 2`.


## Let

Introduce names for expressions by using `let`.

```kuljet
let <name> = <expression> in <expression>
```

e.g.

```kuljet
let x = 1 in x + x
```

## Functions

Write functions with the `fun` keyword:

```kuljet
fun <var> -> <expression>
```

You will sometimes need to provide the type of the function argument using
a *type annotation*:

```kuljet
fun <var>: <type> -> <expression>
```

## Binary Operators

Operate on numbers with `+`, `-`, `*` and `/`.

Compare with `=`, `<`, `>`, `<=` and `>=`.

Combine Boolean values with `and` and `or`.

Concatenate text with `||`.

```kuljet
1 <= 2 and (3 = 3 + 1 or 3 = 3)
```

## If expression

`if` expressions are written:

```kuljet
if <condition> then <expression> else <expression>
```

Unlike C-style languages, they are expressions not statements.


## Then expression

A `then` expression is written:

```kuljet
<expression> [as <name>] then <expression>
```

where the first expression has type `io`. The `then` expression
evaluates the IO action, optionally binding the result to `<name>`.
