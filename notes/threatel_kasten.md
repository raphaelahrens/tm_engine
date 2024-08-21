---
title: Threatelkasten 
keywords: 
  - threat modelling
  - findings
---

In [Findings](./findings.md) it was said that a markdown format should be used to store the findings.

This idea can be taken further, if it is combined with the Zettelkasten approach.


* Every finding is a note (Zettel), which is called a threatel.
* Findings can be linked, to other findings.
* Findings can be linked to proof of concepts
* Findings can be linked to counter meassures
* A finding can have metadata
  * likelihood
  * impact
  * ...

Linking findings has the advantage, that an already existing finding can be used to explain the new finding and so reduce the amount of text.
Further can an attack chain be build by connecting findings.

# Contents of a Finding

* impacted components
* description of the finding
* preconditions necessary 


```markdown
---
finding: <Name of the Finding>
components: 
   - client
   - server
impact: 3
likelihood: 1
---

# Description

Descfription of the finding

# Precodition

For this threat to be explotetd an attacker must have control of the client, for example by [Finding #53](./finding_53.md).

# Counter meassures

To mitigate the threat we use [meassures 1](./m1.md) to reduce the users who have access and [meassure 2](./m2.md) further reduces the impact the operations has.
```
