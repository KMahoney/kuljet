---
title: Standard Library
---

## HTML Tags

The following are HTML functions of type `htmlTag`:

`body`, `html`, `head`, `link`, `p`, `div`, `span`, `a`,
`strong`, `em`, `form`, `input`, `ul`, `ol`, `li`

## Standard Environment

<section>
```
redirect : text -> response
```

Construct a 302 redirect response to the given path.
</section>

<section>
```
file : text -> text -> response
```

`file <contentType> <path>`
constructs a response that serves `<path>` with the given `<contentType>`.
</section>

<section>
```
getTimestamp : io timestamp
```

An IO action to retrieve the current time.
</section>

<section>
```
docType : html
```

The HTML `<!DOCTYPE html>` declaration
</section>

<section>
```
genUUID : io text
```

Generates a unique UUID v4 string
</section>
