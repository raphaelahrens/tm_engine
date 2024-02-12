use petgraph::{
    graph::{Graph, NodeIndex, NodeIndices},
    visit::{EdgeRef, GetAdjacencyMatrix},
    Direction,
};
use std::collections::HashSet;
use std::iter::Peekable;

use crate::{interpreter, Element, Model, QElement, QueryGraph};

fn cmp_weights(query: Option<&QElement>, model: Option<&Element>) -> bool {
    match (query, model) {
        (Some(q_weigth), Some(m_weight)) => interpreter::execute(&q_weigth.0, &m_weight.0)
            .unwrap()
            .into(),
        _ => false,
    }
}

fn degree<N, E>(graph: &Graph<N, E>, node: NodeIndex, direction: Direction) -> usize {
    graph.neighbors_directed(node, direction).count()
}

fn indegree<N, E>(graph: &Graph<N, E>, node: NodeIndex) -> usize {
    degree(graph, node, Direction::Incoming)
}
fn outdegree<N, E>(graph: &Graph<N, E>, node: NodeIndex) -> usize {
    degree(graph, node, Direction::Outgoing)
}

pub fn subgraph_isomorphism(query: &QueryGraph, model: &Model) -> Vec<Vec<NodeIndex>> {
    let mut mapping: Vec<Option<NodeIndex>> = vec![None; query.graph.node_count()];
    let mut all_mappings: Vec<Vec<NodeIndex>> = Vec::new();
    let mut candidates: Vec<HashSet<NodeIndex>> = vec![HashSet::new(); query.graph.node_count()];

    for q_node in query.graph.node_indices() {
        let indeg_q = indegree(&query.graph, q_node);
        let outdeg_q = outdegree(&query.graph, q_node);
        for m_node in model.graph.node_indices() {
            let indeg_m = indegree(&model.graph, m_node);
            let outdeg_m = outdegree(&model.graph, m_node);
            if indeg_q <= indeg_m
                && outdeg_q <= outdeg_m
                && cmp_weights(
                    query.graph.node_weight(q_node),
                    model.graph.node_weight(m_node),
                )
            {
                candidates[q_node.index()].insert(m_node);
            }
        }
    }
    let mut q_nodes = query.graph.node_indices().peekable();
    extend_subgraph_isomorphism(
        query,
        model,
        &mut candidates,
        &mut q_nodes,
        &mut mapping,
        &mut all_mappings,
    );

    all_mappings
}
fn extend_subgraph_isomorphism(
    query: &QueryGraph,
    model: &Model,
    candidates: &mut Vec<HashSet<NodeIndex>>,
    q_nodes: &mut Peekable<NodeIndices>,
    mapping: &mut Vec<Option<NodeIndex>>,
    all_mappings: &mut Vec<Vec<NodeIndex>>,
) {
    let v_qnode = q_nodes.next().expect("There must be a next node");
    for w_mnode in model.graph.node_indices() {
        if candidates[v_qnode.index()].contains(&w_mnode) {
            mapping[v_qnode.index()] = Some(w_mnode);
            let mut next_candidates = candidates.clone();
            for x_qnode in query.graph.node_indices() {
                if x_qnode != v_qnode {
                    next_candidates[x_qnode.index()].remove(&w_mnode);
                }
            }
            for y_mnode in model.graph.node_indices() {
                if y_mnode != w_mnode {
                    next_candidates[v_qnode.index()].remove(&y_mnode);
                }
            }
            if refine_subgraph_isomorphism(query, model, &mut next_candidates, v_qnode, w_mnode) {
                if q_nodes.peek().is_none() {
                    // fully matched
                    let mapping: Option<Vec<_>> = mapping.into_iter().map(|x| *x).collect();
                    all_mappings.push(mapping.unwrap());
                } else {
                    let mut new_q_nodes = q_nodes.clone();
                    extend_subgraph_isomorphism(
                        query,
                        model,
                        &mut next_candidates,
                        &mut new_q_nodes,
                        mapping,
                        all_mappings,
                    )
                }
            }
        }
    }
}

fn refine_subgraph_isomorphism(
    query: &QueryGraph,
    model: &Model,
    candidates: &mut Vec<HashSet<NodeIndex>>,
    v_qnode: NodeIndex,
    w_mnode: NodeIndex,
) -> bool {
    if indegree(&query.graph, v_qnode) > indegree(&model.graph, w_mnode)
        || outdegree(&query.graph, v_qnode) > outdegree(&model.graph, w_mnode)
    {
        dbg!("is this ever true?");
        return false;
    }

    for e_qedge in query.graph.edges_directed(v_qnode, Direction::Incoming) {
        let x_qnode = e_qedge.source();
        for y_mnode in model.graph.node_indices() {
            if !model
                .graph
                .is_adjacent(&model.adjance_matrix, y_mnode, w_mnode)
            {
                candidates[x_qnode.index()].remove(&y_mnode);
            } else {
                if !model.graph.edges_connecting(y_mnode, w_mnode).any(|e| {
                    dbg!(&e.weight());
                    interpreter::execute(&e_qedge.weight().0, &e.weight().0)
                        .unwrap()
                        .into()
                }) {
                    candidates[x_qnode.index()].remove(&y_mnode);
                }
            }
        }
    }
    for e_qedge in query.graph.edges_directed(v_qnode, Direction::Outgoing) {
        let x_qnode = e_qedge.target();
        for y_mnode in model.graph.node_indices() {
            if !model
                .graph
                .is_adjacent(&model.adjance_matrix, w_mnode, y_mnode)
            {
                candidates[x_qnode.index()].remove(&y_mnode);
            } else {
                if !model.graph.edges_connecting(w_mnode, y_mnode).any(|e| {
                    dbg!(&e.weight());
                    interpreter::execute(&e_qedge.weight().0, &e.weight().0)
                        .unwrap()
                        .into()
                }) {
                    candidates[x_qnode.index()].remove(&y_mnode);
                }
            }
        }
    }
    for x_qnode in query.graph.node_indices() {
        if candidates[x_qnode.index()].is_empty() {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::{
        parser::parse_model,
        types::{Class, ClassRef},
        Flow, ModelCompiler, QFlow, QueryGraphBuilder,
    };

    use super::*;
    fn obj() -> ClassRef {
        Rc::new(Class::new("obj"))
    }

    fn create_model() -> ModelCompiler {
        let mut model = ModelCompiler::new(vec![]);

        let ast = parse_model(
            "
                              type Client {
                                  bool: Bool,
                                  client: Bool,
                                  str: String,
                              }
                              type Server {
                                  bool: Bool,
                                  server: Bool,
                              }
                              type Other {}
                              type Flow {}

                              client = Client {
                                  client = true,
                                  bool = true,
                                  str = \"test\",
                              }
                              server = Server {
                                  server = true,
                                  bool = true,
                              }
                              other = Other{}

                              client -> server = Flow {}
                              server -> client = Flow {}
                              ",
        )
        .unwrap();

        model.compile(&ast).unwrap();
        model
    }

    #[test]
    fn test_subgraphisomorphism() {
        let model = create_model();
        let client = model.get_element("client").unwrap();
        let server = model.get_element("server").unwrap();
        let other = model.get_element("other").unwrap();

        let model = model.build();

        let mut query = QueryGraphBuilder::new();
        query.add(QElement::new("true").unwrap());

        let query = query.build();

        assert_eq!(
            subgraph_isomorphism(&query, &model),
            vec![vec![client], vec![server], vec![other]]
        );
    }
    #[test]
    fn test_subgraphisomorphism2() {
        let model = create_model();
        let client = model.get_element("client").unwrap();
        let server = model.get_element("server").unwrap();
        let model = model.build();

        let mut query = QueryGraphBuilder::new();
        let s = query.add(QElement::new(".server").unwrap());
        let c = query.add(QElement::new(".client").unwrap());
        let result_login = query.connect(c, s, QFlow::new("true").unwrap());
        let result_login = query.connect(s, c, QFlow::new("true").unwrap());

        let query = query.build();

        assert_eq!(
            subgraph_isomorphism(&query, &model),
            vec![vec![server, client]]
        );
    }
    #[test]
    fn test_subgraphisomorphism3() {
        let mut model = ModelCompiler::new(vec![]);
        let client = model.add(Element::new(obj()));
        let server = model.add(Element::new(obj()));
        let other = model.add(Element::new(obj()));

        let request_login = model.connect(client, server, Flow::new(obj()));
        let result_login = model.connect(client, server, Flow::new(obj()));
        let result_login2 = model.connect(server, client, Flow::new(obj()));
        let model = model.build();

        let mut query = QueryGraphBuilder::new();
        let s = query.add(QElement::new("true").unwrap());
        let c = query.add(QElement::new("true").unwrap());
        let o = query.add(QElement::new("true").unwrap());
        let result_login = query.connect(c, s, QFlow::new("true").unwrap());
        let result_login = query.connect(c, s, QFlow::new("true").unwrap());

        let query = query.build();

        assert_eq!(
            subgraph_isomorphism(&query, &model),
            vec![vec![server, client, other]]
        );
    }
    #[test]
    fn test_subgraphisomorphism4() {
        let mut model = ModelCompiler::new(vec![]);
        let broker = model.add(Element::new(obj()));
        let publisher = model.add(Element::new(obj()));
        let c1 = model.add(Element::new(obj()));
        let c2 = model.add(Element::new(obj()));
        let c3 = model.add(Element::new(obj()));
        let c4 = model.add(Element::new(obj()));
        let c5 = model.add(Element::new(obj()));
        let c6 = model.add(Element::new(obj()));

        model.connect(publisher, broker, Flow::new(obj()));
        model.connect(broker, c1, Flow::new(obj()));
        model.connect(broker, c1, Flow::new(obj()));
        model.connect(broker, c2, Flow::new(obj()));
        model.connect(broker, c3, Flow::new(obj()));
        model.connect(broker, c4, Flow::new(obj()));
        model.connect(broker, c5, Flow::new(obj()));
        model.connect(broker, c6, Flow::new(obj()));
        let model = model.build();

        let mut query = QueryGraphBuilder::new();
        let b = query.add(QElement::new("true").unwrap());
        let p = query.add(QElement::new("true").unwrap());
        let s = query.add(QElement::new("true").unwrap());
        query.connect(p, b, QFlow::new("true").unwrap());
        query.connect(b, s, QFlow::new("true").unwrap());

        let query = query.build();

        assert_eq!(subgraph_isomorphism(&query, &model).len(), 6);
        assert_eq!(
            subgraph_isomorphism(&query, &model),
            vec![
                vec![broker, publisher, c1],
                vec![broker, publisher, c2],
                vec![broker, publisher, c3],
                vec![broker, publisher, c4],
                vec![broker, publisher, c5],
                vec![broker, publisher, c6],
            ]
        );
    }
}
