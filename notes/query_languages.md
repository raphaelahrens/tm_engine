---
title: PyTM clone -- Query langauges
keywords: 
  - threat modelling
  - pytm 
  - query languages 
---

Most of PyTMs queries are simple attributes checks of the current node.
For example

```
"target.usesXMLParser is False or target.controls.disablesDTD is False"
"target.controls.validatesInput is False or target.controls.sanitizesInput is False"
```

These queries are Python code which are executed inside the pytm process.
This design decision requires that the queries are trusted by the executer of the threat model.

A more flexible approach would be to use some query language for a generic data structure like JSON combined with a logic language.


Possible solutions

- JSON Path
- jq query langauge

pytm distinguishes queries for different "targets" for example some threats only apply for processes, servers or datastores.
This seems to be an optimisation, so the query only has to be applied to the sub list of the target.
The same result could be achieved by testing the targets type.

# Examples queries

```json_path
$.has_file_access == true and $.accepts_file_path == true
```

```json_path
$.type is WebServer and ($.port == 80 or $.tlsVersion < config.MinTLS)
```
