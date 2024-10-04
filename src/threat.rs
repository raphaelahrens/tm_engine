use std::{collections::HashMap, ffi::OsStr, path::Path};

use serde::Deserialize;

use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    Graph,
};
use threatmd::{
    HeadingLevel,
    MarkdownIter,
    MarkdownParser,
};
use crate::{parser::parse_query, types::{TypeError, TypeTable}};


use crate::{interpreter, parser::{self, QNode, compile_query}};

const QUERY_EXT: &str = "tqmd";


#[derive(thiserror::Error, Debug)]
pub enum ThreatError {
    #[error("Metadata Error")]
    MetadataParsingError(#[from] toml::de::Error),
    #[error("Markdown Error")]
    ThreatMdError(#[from] threatmd::Error),
    #[error("Type Error")]
    TypeError(#[from] TypeError),
    #[error("IO Error")]
    IoError(#[from] std::io::Error),
    #[error("Parsing error {context:?}\n\n\t {msg}")]
    ParseError{
        context: winnow::error::ContextError,
        msg: String
    },
    #[error("Received an undeclared element {got}")]
    UndelcraedElement{
        got: String,
    }

}

fn from_parse_error(e: parser::ParseError, query_lang: &str) -> ThreatError {
            let offset = e.offset();
            ThreatError::ParseError{
                context:e.into_inner(),
                msg: parser::get_line_from_offset(query_lang, offset).to_string()
            }
}

#[derive(Deserialize, PartialEq, Debug)]
struct MetaField{
    sid: String,
    severity: String,
    target: Vec<String>,
    likelihood: String,
}

/**
 * A [QueryGraph] is a graph similar to a [super::Model] with the difference that the nodes ([QNode]) and
 * edges ([QFlow]), do not contain data but queries [interpreter::Query].
 *
 * Each query of a [QNode] in the the [QueryGraph] can match to zero or more [super::Element]s in the [super::Model] and each [QFlow] can match to zero or more [super::Flow]s, which are between already matched [super::Element]s.
 *
 * So with a [QueryGraph] it is possible to find mutiple matching sub-graphs in the [super::Model].
 */
#[derive(Debug)]
pub struct QueryGraph {
    pub graph: Graph<QElement, QFlow>,
}

impl QueryGraph {
    pub fn from_tql(query_lang: &str, types: &TypeTable) -> Result<Self, ThreatError> {
        let nodes = parser::parse_tql(query_lang).map_err(|e|{
            from_parse_error(e, query_lang)
        })?;
        
        let mut  builder = QueryGraphCompiler::new(&types);

        let type_table = TypeTable::new();
        
        for node in nodes {
            match node {
                    QNode::Element{
                        name,
                        datatype: _, //TODO ignore for now
                        query,
                        doc_str: _,
                    } => {
                        let e =QElement(compile_query(query, &type_table)?);
                        builder.add(e, name);
                    },
                    QNode::Flow{
                        from,
                        to,
                        datatype: _, //TODO ignore for now
                        query,
                        doc_str: _,
                    } => {
                        let flow = QFlow(compile_query(query, &type_table)?);
                        builder.connect_names(from, to, flow)?;
                    },
                    QNode::Import{
                        name: _,
                    } => {
                    }
            }
        }
        Ok(builder.build())
    }
}

/**
 * The [QueryGraphCompiler] is mutable version of the [QueryGraph], which will be return after
 * calling [Self::build].
 *
 * compared to the [QueryGraph] the [QueryGraphCompiler] has a larger inernal state, which is droped
 * after calling build.
 */
pub struct QueryGraphCompiler<'tt> {
    graph: Graph<QElement, QFlow>,
    elements: HashMap<String, NodeIndex>,
    types: &'tt TypeTable,
}
impl <'tt> QueryGraphCompiler<'tt> {
    pub fn new(types: &'tt TypeTable) -> Self {
        Self {
            graph: Graph::new(),
            elements: HashMap::new(),
            types,
        }
    }

    pub fn add_unnamed(&mut self, query: &str) -> Result<NodeIndex, ThreatError> {
        let query = parse_query(query).map_err(|e | from_parse_error(e, query))?;
        let query = compile_query(query, self.types)?;
        let e = QElement(query);
        Ok(self.graph.add_node(e))
    }

    pub fn add(&mut self, e: QElement, name: &str) -> Option<NodeIndex> { 
        if self.elements.contains_key(name){
            return None;
        }
        let idx = self.graph.add_node(e);
        self.elements.insert(name.to_string(), idx);
        Some(idx)
    }
    

    pub fn connect_names(&mut self, a: &str, b: &str, flow: QFlow) -> Result<EdgeIndex, ThreatError> {
        let a = *self.elements.get(a).ok_or(ThreatError::UndelcraedElement{got: a.to_string()})?;
        let b = *self.elements.get(b).ok_or(ThreatError::UndelcraedElement{got: b.to_string()})?;

        Ok(self.graph.add_edge(a, b, flow))
    }

    pub fn connect(&mut self, a: NodeIndex, b: NodeIndex, query: &str) -> Result<EdgeIndex, ThreatError> {
        let query = parse_query(query).map_err(|e | from_parse_error(e, query))?;
        let query = compile_query(query, self.types)?;
        let flow = QFlow(query);
        Ok(self.graph.add_edge(a, b, flow))
    }
    pub fn build(self) -> QueryGraph {
        QueryGraph {
            graph: self.graph,
        }
    }
}

#[derive(Debug)]
pub struct QElement(pub interpreter::Query);

impl QElement {
}

#[derive(Debug)]
pub struct QFlow(pub interpreter::Query);
impl QFlow {
}
#[derive(Debug)]
pub struct Threat{
    pub sid: String,
    pub severity: String,
    pub target: Vec<String>,
    pub description: String,
    pub details: String,
    pub example: String,
    pub mitigations: String,
    pub condition: QueryGraph,
    pub references: String,
    pub prerequisites: String,
    pub likelihood: String,
}

pub fn parse_tqmd(markdown_input: &str, types: &TypeTable) -> Result<Threat, ThreatError>{
    let p = MarkdownParser::new(markdown_input);
    let mut parser = p.iter();

    let metadata = parser.metadata()?;

    let metadata: MetaField = toml::from_str(&metadata)?;

    let description = parser.heading(HeadingLevel::H1 )?;


    let details = p.to_string(parser.multi(MarkdownIter::text));

    parser.named_heading(HeadingLevel::H2, "Example")?;
    let example = p.to_string(parser.multi(MarkdownIter::text));

    parser.named_heading(HeadingLevel::H2, "Mitigations")?;
    let mitigations = p.to_string(parser.multi(MarkdownIter::text));

    parser.named_heading(HeadingLevel::H2, "Condition")?;
    let condition = p.get_text(parser.lang_block("tquery")?);
    let condition = QueryGraph::from_tql(&condition, &types)?;

    parser.named_heading(HeadingLevel::H2, "Prerequisites")?;
    let prerequisites = p.to_string(parser.multi(MarkdownIter::text));
    parser.named_heading(HeadingLevel::H2, "References")?;
    let refernces = parser.item_list()?;


    Ok(Threat{
        sid: metadata.sid,
        severity: metadata.severity,
        target: metadata.target,
        likelihood: metadata.likelihood,
        description,
        details,
        example,
        mitigations,
        condition,
        prerequisites,
        references: refernces.join(", "),
    })
}

pub fn load_threats(threat_path: &Path, types: &TypeTable) -> Result<Vec<Threat>, ThreatError> {

    let mut threats = Vec::new();

    for entry in threat_path.read_dir()? {
        if let Ok(entry) = entry {
            if entry.file_type()?.is_file() && entry.path().extension() == Some(OsStr::new(QUERY_EXT)){
                let md = std::fs::read_to_string(&entry.path())?;
                let t = parse_tqmd(&md, types)?;
                threats.push(t);
            }
        }
    }

    Ok(threats)
}

#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn example_threat() {
        let tql_str = "
import dfd

gateway = Process(true)
service = Process(true)
client = ExternalEntity(true)

client -> gateway = Flow(true)
gateway -> service = Flow(true)
service -> gateway = Flow(true)
gateway ->  client = Flow(true)
        ";
        let types = TypeTable::new();
        let threat_condition = QueryGraph::from_tql(tql_str, &types).expect("A parsable QueryGraph");

        assert_eq!(threat_condition.graph.node_count(), 3);
        assert_eq!(threat_condition.graph.edge_count(), 4);

    }

    #[test]
    fn example_tqmd() {
        let tql_str = "
---
sid = 'AA01'
severity = 'Medium'
target = ['Server', 'Process']
likelihood = 'Low'
---

# Authentication Abuse/ByPass

An attacker obtains unauthorized access to an application, service or device either through knowledge of the inherent weaknesses of an authentication mechanism, or by exploiting a flaw in the authentication scheme's implementation. In such an attack an authentication mechanism is functioning but a carefully controlled sequence of events causes the mechanism to grant access to the attacker. This attack may exploit assumptions made by the target's authentication procedures, such as assumptions regarding trust relationships or assumptions regarding the generation of secret values. This attack differs from Authentication Bypass attacks in that Authentication Abuse allows the attacker to be certified as a valid user through illegitimate means, while Authentication Bypass allows the user to access protected material without ever being certified as an authenticated user. This attack does not rely on prior sessions established by successfully authenticating users, as relied upon for the Exploitation of Session Variables, Resource IDs and other Trusted Credentials attack patterns.

## Example

test

## Mitigations

Use strong authentication and authorization mechanisms. A proven protocol is OAuth 2.0, which enables a third-party application to obtain limited access to an API.

## Condition

```tquery
p = Process(.controls.authenticatesSource == false)
```

## Prerequisites

An authentication mechanism or subsystem implementing some form of authentication such as passwords, digest authentication, security certificates, etc. which is flawed in some way.

## References

- https://capec.mitre.org/data/definitions/114.html
- http://cwe.mitre.org/data/definitions/287.html
";
        let types = TypeTable::new();
        let threat_md = parse_tqmd(tql_str, &types).expect("This should parse");
        
        assert_eq!(threat_md.example, "test");

        assert_eq!(threat_md.condition.graph.node_count(), 1);

    }

}

