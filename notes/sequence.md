---
title: PyTM clone -- Sequence
keywords: 
  - threat modelling
  - pytm 
  - dataflow
---

A Sequence is a set of dataflows, which have a time ordering.
The Sequence is triggered by a single event, which results in the first dataflow.
The first flow triggers an event for the second dataflow and so one.

A typical sceanrio is a Web client making an HTTP request, which causes a database lookup, which returns and lead to a HTTP reply.

```python
web_client = Process()
web_server = Server()
db = Datastore()

seq1 = Sequence(
    Dataflow(web_client, web_server, "HTTP Request"),
    Dataflow(web_server, db, "Data Query"),
    Dataflow(db, web_server, "Data result"),
    Dataflow(web_server, web_client, "HTTP Reply"),
)
```

The advantage of modelling this in a Sequnece is that a threat beginning in one dataflow of the Sequence is affecting all following dataflows.
Which then can be better visualized.
A threat inside a sequence has can be influenced by the preceding and succeeding flows.
