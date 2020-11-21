---
title: Future Work
---

## JSON

It would be nice to be able to create simple JSON APIs using Kuljet. It could
automatically translate queries into JSON, such as:

```kuljet
table example { x : text }

serve get / =
  json example
```

It would also be straightforward to create an OpenAPI spec of the service, given
that the types are known.

It would be useful to have some syntax for responding to `application/json`
requests.


## Automatic Formatting

After using a language with a good automatic formatter, such as Elm, it is difficult
to go back to formatting programs by hand. The reason Kuljet doesn't have comments
yet is to make this job easier.
