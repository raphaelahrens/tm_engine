---
title: PyTM clone -- Remarks
keywords: 
  - threat modelling
  - pytm 
  - remarks
---

Not everything is a finding, maybe it would be useful to also add remarks to the hreat model.

A remark can be something like an advice for a technologie.

For example if a a software is written in C it is avisable to run code analysis software.
When using JSON it is advisable to restrict the maximum resources of the dessiralizer and the maximum lenght of an JSON object.

Maybe a remark is just a small finding?

Remarks could also be similar to assumtions.

So if we use TLS we assume that secure certificates are used, but this does not have to be true.
This assumtion should be documented somehow.
