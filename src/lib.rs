use std::collections::BTreeMap;

pub mod parser;
pub mod interpreter;

use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    Graph,
};

pub enum Value {
    Bool(bool),
    Number(i64),
    String(String),
    Array(Vec<Value>),
    Object(ValueTree),
}

type ValueTree = BTreeMap<String, Value>;

pub struct Element(ValueTree);

impl Element {
    pub fn new() -> Self {
        Self(ValueTree::new())
    }
}

pub struct Flow(ValueTree);

impl Flow {
    pub fn new() -> Self {
        Self(ValueTree::new())
    }
}

pub struct Model {
    graph: Graph<Element, Flow>,
}

impl Model {
    pub fn new() -> Self {
        Model {
            graph: Graph::new(),
        }
    }
    pub fn add(&mut self, e: Element) -> NodeIndex {
        let idx = self.graph.add_node(e);
        idx
    }

    pub fn connect(&mut self, a: NodeIndex, b: NodeIndex, flow: Flow) -> EdgeIndex {
        let idx = self.graph.add_edge(a, b, flow);
        idx
    }
}

pub struct Query {
    graph: Graph<QElement, QFlow>,
}

pub struct QValue();

pub struct QElement();
pub struct QFlow();

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_create_model() {
        let mut model = Model::new();
        let client = model.add(Element::new());
        let server = model.add(Element::new());

        let request_login = model.connect(client, server, Flow::new());
        let result_login = model.connect(server, client, Flow::new());
    }
}
