---
title: PyTM clone -- Custom Query langauge
keywords: 
  - threat modelling
  - pytm 
  - query language 
---

# Symbols for the current object

* `$.`
* `@.`
* `§.`
* `*.`
* `?.`
* `0.`
* `#.`
* `>.`
* ` .`

# PEG

```pest
Query = _{ SOI ~ Expr ~ EOI }

Expr = { NotExpr | LogicExpr | Term }

Term = _{ Member | Const | "(" ~ Expr ~ ")" }

NotExpr = { "!" ~ Term }

LogicExpr = { CompExpr ~ (LogicOperator ~ CompExpr)* }

LogicOperator = @{ "and" | "or" }

CompExpr = { Term ~ (CompOperator ~ Term)? }

CompOperator = @{  "<" | "<=" | "==" | "!=" | ">=" | ">"}

Member = ${ ("." ~ Identifier)+}

Identifier = @{ASCII_ALPHA ~ ASCII_ALPHANUMERIC*}

Const = _{ Int | QuotedAtom | QuotedString }

Int = @{  ASCII_DIGIT+ }

QuotedAtom = ${ "'" ~ Atom }

Atom = @{  ASCII_ALPHA ~ ASCII_ALPHANUMERIC*}

QuotedString = ${ "\"" ~ String ~ "\""}

String = @{  ASCII_ALPHANUMERIC* }

WHITESPACE = _{ " " | "\t" | "\n" }


```

# Examples queries

```
.has_file_access == true and .accepts_file_path == true
```

```
.type == 'WebServer and (.port == 80 or .tlsVersion < CONFIG.minTLS)
```

```
.neighbors().inlcudes
```

```
let query = Query()
let a = query.element(.tlsVersion < CONFIG.minTLS)
let b = query.external()
let req = query.flow(a, b, .crosses_border() )


```
