---
title: Isomorphism and threat modelling
keywords:
  - threat modelling
  - isomorphismn
---

In threat modelling we first define a model of the software we want to build or analyze.
Often these models are in the form of a data flow diagrams (DFD), a sequence diagram or process flow diagramms or Buisness Process Model and Notation(BPMN).
Basicly any digram, which can visualize the information flow in an information processing system can be used.

The tradition is to use DFD, possibly influenced by Shostack[@shostack] and co.
Following that tradtion I will use for my examples DFD, but the general principles should also apply to the other kinds of diagramms.

When we create a model of the system, we model the flow of information through the components of the system.
In a DFD the components either a process, a datastore or an external entity are represented by nodes.
The information flow is modeled by drawing directed edges between the nodes indicating, that information is transfered from one node to another in the given direction.
Basicaly we create a graph G=(N,E) where N are the nodes and E are an ordered pair of verticies.

Lets look at an example.

```
# The customer buying on our webshop using a web browser
customer = ExternalEntity

# A sales API for the Webshop
sale_api = Process

# A Kafka broker
broker = Process

# The payment handler receives and stores the orders and gets notified if payment was conducted
payment_handler = Process

# Database for storing open and past orders
payment_db = Datastore

# External Payment servie provider
extern_payment = ExternalEntity 

# Shipping service that manages the information for the shipping process of the warehouse
shipping = Process

# Customer sends an order to the sales Api
customer -> sale_api = Http.Post{
    # A list of items and an identifier for the customer
    data = Data{
        format = JSON,
        authenicated,
    }
}

# The Sales API forwards the order to the payment service for processing via the broker"
sale_api -> broker =  Kafka.Publish{
    topic = "ordering.payment",
    # A list of items and an identifier for the customer"
    data = Data {
        format = CBOR
    }
}

# The Broker forwards the the order to one payment process",
broker -> payment_handler = Kafka.Publish {
    topic = "ordering.payment",

    # A list of items and an identifier for the customer"
    data = Data {
        format = CBOR
    }
}

# the payment process stores the order
payment_handler -> payment_db = SQLInsert

# Send the order to external payment services
payment_handler -> extern_payment = Flow

# If the the payment was succesfully made the external payment service sends this positive reply.
extern_payment -> payment_handler = Flow

# The payment Handler Selects the orginal order from the database
payment_handler -> payment_db = SQL

# The oder as a result of the SQL Select
payment_db -> payment_handler = SQL

# send the oder to the shipping service
payment_handler -> broker = Kafka.Publish

# receive the shipping information and start shipping
broker -> shipping = Kafka.Publish
```
