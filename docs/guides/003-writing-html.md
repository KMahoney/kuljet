---
title: Writing HTML
---

Strings, numbers and lists of HTML are sub-types of `html`,
so you can write HTML like this:

| expression | HTML |
| ---------- | ---- |
| 1 | 1 |
| "Hello" | Hello |
| "&lt;unsafe&gt;" | &amp;lt;unsafe&amp;gt; |
| [1, 2, 3, "Hello"] | 123Hello |


You can build HTML using tag functions such as `p` and `div`, and they
can optionally be provided attributes and child HTML.

| expression | HTML |
| ---------- | ---- |
| div | &lt;div&gt;&lt;/div&gt; |
| div "hello" | &lt;div&gt;hello&lt;/div&gt; |
| div { class = "someclass" } | &lt;div class="someclass"&gt;&lt;/div&gt; |
| div { class = "someclass" } "hello" | &lt;div class="someclass"&gt;hello&lt;/div&gt; |
| div [ div 1, div 2 ] | &lt;div&gt;&lt;div&gt;1&lt;/div&gt;&lt;div&gt;2&lt;/div&gt;&lt;/div&gt; |

Attributes are records with fields of type `text`.
