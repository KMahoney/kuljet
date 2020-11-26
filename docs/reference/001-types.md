---
title: Types
---


## Basic Types

- `unit`
- `int`
- `bool`
- `text`
- `timestamp`
- `response`

## HTML Types

- `html`
- `htmlTag`
- `htmlTagAttrs`

valid subtypes of `html` are:

- `int`
- `text`
- `htmlTag` 
- `htmlTagAttrs`
- `list html`

The are special rules for `htmlTag` and `htmlTagAttrs` application:

| Type | Applied Type | Result Type |
| ---- | ------------ | ----------- |
| htmlTag | attributes | htmlTagAttrs |
| htmlTag | html | html |
| htmlTagAttrs | html | html |

Where `attributes` is any record type with `text` fields.

Examples:

| Expression | Type |
| ---------- | ---- |
| div | htmlTag |
| div { class = "x" } | htmlTagAttrs |
| div "x" | html |
| div { class = "x" } "x" | html |

## List Types

List types are written `list <type>`, e.g. `list int`.

## Record Types

Record types are written with curly braces:

```
{ <field> : <type>, ... }
```

## Function Types

Function types are written with the infix arrow constructor `->`, e.g.

```
int -> int -> bool
```

It is right associative, so this is equivalent to:

```
int -> (int -> bool)
```

## Database Types

Basic types you can store in the database include:

- `int`
- `bool`
- `text`
- `timestamp`


## Query Types

Query types are written:

```
query { <field> : <type>, ... }
```

The record type is the type of the query *row*, so the yield operator will
transform a `query` into a `list`.


## IO types

IO types are written `io <type>` and represent an action which can
be executed by the `<io-expression> [as <name>] then <expression>`
form.
