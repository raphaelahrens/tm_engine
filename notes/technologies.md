---
title: PyTM clone -- Technologies
keywords: 
  - threat modelling
  - pytm 
  - findings
  - technology
---

Some elments or flow make use of a specific technologie like JSON, nginx, HTTP, ...

Each have there own set of possible findings, an as soon as one technology is used these should be applied to the threat model.

# Example findings

```markdown
---
id: web_webserver/image_storage
elements: 
  - web server
  - image storage
source: client
type: CWE-73
---

The web server does not verify if a file path provided by the client is accessing a file outside the image folder.

```
