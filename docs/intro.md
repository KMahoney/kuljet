---
title: Introduction
---

**[A WORK IN PROGRESS]**

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

## Installation

* [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
* Clone the [repository](https://github.com/KMahoney/kuljet) and change to its directory.
* `stack install kuljet`

You should now have the `kuljet` command available.

Make sure you have the output of `stack path --local-bin` in your `PATH`.

## Hello World

```kuljet
serve get / = "Hello World!"
```

## Example

Here's a [simple chat application](https://chat.kuljet.com)

```kuljet
table messages { message : text, timestamp : timestamp }

serve get / =
  let template = fun content ->
    html [ head (link { href = "/style.css", rel = "stylesheet" })
         , body content
         ]
  in
  let messagesHtml =
    let query = (messages order timestamp desc limit 100) order timestamp asc in
    div { class = "messages" }
      (query select { message } -> div { class = "message" } message)
  in
  let formHtml =
    let formAttrs = { method = "POST", action = "/msg", class = "inputForm" } in
    let nameInput = input { name = "message", class = "inputMessage", autofocus = "true" } in
    let submitInput = input { type = "submit" } in
    form formAttrs [ nameInput, submitInput ]
  in
  template [ messagesHtml, formHtml ]

serve post /msg =
  fun formVars: { message: text } ->
    insert messages { message = formVars.message, timestamp = now } then
    redirect "/"

serve get /style.css =
  file "text/css" "style.css"
```

### Running the Example

```
cd examples/chat && kuljet serve chat.kj
```
