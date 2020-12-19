---
title: Writing HTML
---

Strings, numbers and lists of HTML are sub-types of `html`,
so you can write HTML like this:

| expression | HTML output |
| ---------- | ----------- |
| 1 | 1 |
| "Hello" | Hello |
| "&lt;unsafe&gt;" | &amp;lt;unsafe&amp;gt; |
| [1, 2, 3, "Hello"] | 123Hello |


Any name enclosed in angle brackets is a *tag function* that has
type `htmlTag`. You can build HTML using tag functions such as `<p>`
and `<div>`, and they can optionally be provided attributes and child
HTML.

| expression | HTML output | type |
| ---------- | ----------- | ---- |
| &lt;div&gt; | &lt;div&gt;&lt;/div&gt; | htmlTag |
| &lt;div&gt; "hello" | &lt;div&gt;hello&lt;/div&gt; | html |
| &lt;div&gt; { class = "someclass" } | &lt;div class="someclass"&gt;&lt;/div&gt; | htmlTagWithAttrs |
| &lt;div&gt; { class = "someclass" } "hello" | &lt;div class="someclass"&gt;hello&lt;/div&gt; | html |
| &lt;div&gt; [ div 1, div 2 ] | &lt;div&gt;&lt;div&gt;1&lt;/div&gt;&lt;div&gt;2&lt;/div&gt;&lt;/div&gt; | html |

Attributes are records with fields of type `text`.
