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
now : timestamp
```

The time the current request was made.
</section>

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
bindMaybe : maybe 'a -> ('a -> maybe 'b) -> maybe 'b
```

Chain `maybe` values.
</section>

<section>
```
listHead : list 'a -> maybe 'a
```

The first element of a list.
</section>

<section>
```
true : bool
false : bool
not : bool -> bool
```

Boolean logic.
</section>

### Text

<section>
```
textLength : text -> int
```

The length of the given text.
</section>

<section>
```
regexpMatch : text -> text -> bool
```

`regexpMatch regexp text` tests `text` against `regexp`.
</section>

<section>
```
commonMark : text -> html
```

Parse text as 'common mark'.
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
genUUID : io text
```

Generates a unique UUID v4 string
</section>

<section>
```
randomBytes : int -> io text
```

Generates N random bytes, base64 encoded. This is useful for generating session keys.
</section>

### Passwords

<section>
```
hashPassword : text -> password
```

Hashes a password.
</section>

<section>
```
validatePassword : text -> password -> bool
```

Check a password against a password hash
</section>
