---
title: Threat model engine
keywords: 
  - threat modelling
  - pytm clone
  - engine
---

# Problem

PyTM has the problem that the Model representation and the description of the model are to tightly coupled.

This makes it unnecessarily difficult to create new nodes/Dataflows and Objects.

The Idea is to replace the internal representation with a loosely typed one.

# Possible representaions 

* Graph database
  * 
* hand written graph plus "JSON"- data structure


This new threat model engine has the advantage, that it is language independent, meaning a model can be written in Lua, Python or any other language with proper bindings.

# Lua syntax example

```lua
require "tm"

local bib = tm.load_elements("/path/to/some/elements/folder")
local threats = tm.load_threads("/path/to/some/threats/folder")

local customer = bib.actor "Customer"
local browser = bib.browser()


local webserver = bib.server()


retrun tm.process(
    {
        bib.flow(customer, browser, {}),
        bib.flow(browser, webserver),
        bib.flow(webserver, browser),
    },
    threats
)

```

```python
import tm

bib = tm.load_elements("/path/to/some/elements/folder")
threats = tm.load_threads("/path/to/some/threats/folder")

customer = bib.actor("Customer")
browser = bib.browser()


webserver = bib.server()


retrun tm.process(
    [
        bib.flow(customer, browser),
        bib.flow(browser, webserver),
        bib.flow(webserver, browser),
    ],
    threats
)

```
