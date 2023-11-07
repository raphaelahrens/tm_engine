---
title: PyTM clone -- Counter measures
keywords: 
  - threat modelling
  - pytm 
  - counter measures
  - controls
---

Controls or counter measures are very similar to findinds in that they should not be part of the code base, but be in a seperate library.

Since every measure or control sets a specific set of properties to the element where it is implemnted, these properties should be described with the couter measure.

How can we describe a control?

It needs
 - a name,
 - an identifier
 - a description
 - a set of element types where it can be applied
 - a set of properties it gives to the element
     - which can be positive properties TLS1.2 => `isEncrypted=true`
     - or negative (Maybe not for TLS)
     - or new findings TLS 1.2 can be downgraded
 - 


```markdown
---
id: C-100
name: TLS1.2
elements: 
  - flow
  - image storage
properties:
  isEncrypted: true
  findings:
    - BLA
    - BLUP
---

Transport Layer Security protocol version 1.2

# Links

```

