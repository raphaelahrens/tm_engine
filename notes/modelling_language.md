---
title: modelling language
keywords:
  - threat modelling
  - pytm
  - modelling language
---

# Features

The main task of the modeling language is it to create a model based on a graph datastructure.
Therfore the langauge needs to be able to represent node and directed edges.

Each node can have assosiated data which describes the node.
To connect the nodes inside the graph the nodes first have to be created, so they can later be used.

The edges of the graph are a combination of two nodes with a direction and with additional .

```

Data:Object {
    sensitivity: Atom,
    format: Atom,
    description: String,
    target_value: Atom,
}

Credential:Data {
    sensitivity = 'High,
    format = 'UTF-8,
    taget_value = 'High
}

module HTTP {
    Method:Enum {
        'GET,
        'POST,
        'PUT,
        'HEAD,
        'DELETE,
        'CONNECT,
        'OPTIONS,
        'TRACE,
        'PATCH,
    }
    Request:Object {
        method: Method = 'GET
        path: String
        headers: [Header] = []
        data: Data
    }
    Response:Object {
        headers: [Header] = []
        data: Data
    }
}

module MQTT {
    ///This module is a representaion of the MQTT protocol
    Publish: Object {
        /// A publish message as described by the MQTT protocol
        payload: Data,
        QoS: int,
        topic: String

    }
}

File:Datastore{
    path:String,
    data:Data,

}

f = File{
    path = "/etc/server/user.passwd"
    read = true
    write = true
    execute = false
    data = Data {
        format = 'Text,
        sesitivity = 'High,
    }
}

s = Server{
    usesTLS,
    usesXMLParser,
    hasAccessControl,
    validatesInput,
}

broker = Server{
    protocol = 'MQTT
    usesTLS,
}

c = Client {
    
}

c -> s = HTTP.Request {
    data = Credential{
        description = "user credetnials"
    }
}

s -> b = MQTT.Publish {
    topic = "log/authN"
    data = 
}

c <- s = HTTP.Response {
    data = Data {
        description = "
    }
}

c =>> s = (Data{}, 
Data{})
```
