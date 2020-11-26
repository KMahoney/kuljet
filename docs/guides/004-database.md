---
title: Interacting with the Database
---

## Declaring Tables

To query a table, specify the type of its fields with a `table` declaration.
As of writing, valid types for fields are `text`, `int` and `timestamp`.

```kuljet
table example { textField: text, intField: int, tsField: timestamp }
```

## Yield

The yield operator `->` executes a query, such as a table, and maps it to a Kuljet expression.
In the expression, fields of the query can be referenced by name.

For example, 

```kuljet
table ints { i: int }

serve get / = ul (ints -> li i)
```

If table `ints` contained values 1 to 3, the output would be:

```html
<ul><li>1</li><li>2</li><li>3</li></ul>
```


## Relational Operators

Tables can be queried with the `select`, `where`, `order` and `limit` operators.

<section>

```kuljet
example select { textField, intField = intField + 1 }
```

is equivalent to the SQL:

```sql
SELECT textField, intField + 1 AS intField FROM example
```

Note that you can name pun fields, so `select { textField }` is the same as `select { textField = textField }`.
</section>

<section>

```kuljet
example where intField = 1
```

is equivalent to the SQL:

```sql
SELECT * FROM example WHERE intField = 1
```
</section>

<section>

```kuljet
example order intField desc
```

is equivalent to the SQL:

```sql
SELECT * FROM example ORDER BY intField DESC
```
</section>

<section>

```kuljet
example limit 100
```

is equivalent to the SQL:

```sql
SELECT * FROM example LIMIT 100
```
</section>

<section>
```kuljet
example1 natJoin example2
```

is equivalent to the SQL:

```sql
SELECT * FROM example1 NATURAL JOIN example2
```

Both tables will need at least one field in common to perform a natural join,
or the expression will result in a type error.
</section>

## Inserting Data

An `insert` expression has the syntax:

```kuljet
insert <tableName> <expression>
```

where `<expression>` is a record matching the type of a table row.

An insert expression has type `io unit`, meaning it is an IO action 
that can be executed with the `<io-expression> then <expression>` form.

It is common to insert data in response to a POST request, and then redirect:

```kuljet
table example { a: text, b: text }

serve post / =
  fun data: { a: text, b: text } ->
     insert example data then
     redirect "/"
```
