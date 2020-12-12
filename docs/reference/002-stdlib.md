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
emptyHtml : html
```

Blank HTML.
</section>

<section>
```
docType : html
```

The HTML `<!DOCTYPE html>` declaration
</section>

<section>
```
cookie : text -> maybe text
```

Retrieve the contents of a browser cookie.
</section>

<section>
```
addCookie : response -> text -> text -> response
```

`addCookie <response> <key> <value>` adds a cookie to a response.
</section>

<section>
```
maybe : maybe 'a -> 'b -> ('a -> 'b) -> 'b
```

Branch on a `maybe` type.
</section>

<section>
```
listHead : list 'a -> maybe 'a
```

The first element of a list.
</section>

### IO Actions

<section>
```
liftIO : 'a -> io 'a
```

Lift a pure expression into `io`.
</section>

<section>
```
getTimestamp : io timestamp
```

An IO action to retrieve the current time.
</section>


<section>
```
genUUID : io text
```

Generates a unique UUID v4 string
</section>

