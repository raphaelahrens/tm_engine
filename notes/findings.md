---
title: PyTM clone -- Findings
keywords: 
  - threat modelling
  - pytm 
  - findings
---

To document the findings of a threat model a simple markdown file can be used.

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
