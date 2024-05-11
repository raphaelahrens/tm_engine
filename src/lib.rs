use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet}, fs::File, io::Read, path::{Path, PathBuf}
};

use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    visit::GetAdjacencyMatrix,
    Graph,
};

use fixedbitset::FixedBitSet;

use thiserror::Error;
use line_span::LineSpanExt;

mod algo;
mod interpreter;
pub mod parser;
mod types;

pub use interpreter::{Decision, ExecutionError, Reason};

pub use parser::Stream;


use self::{parser::{ClassMember, Constant, Datatype, MemberValue, Node, ObjectMember}, types::UnionType};

const EXTENSION: &str = "tm";

type Str = Box<str>;

#[derive(Error, Debug, PartialEq)]
pub enum RunError {
    #[error("Parsing Error")]
    ParseError(winnow::error::ErrMode<winnow::error::ContextError>),
}
impl<'nom> From<winnow::error::ErrMode<winnow::error::ContextError>> for RunError {
    fn from(sub: winnow::error::ErrMode<winnow::error::ContextError>) -> Self {
        Self::ParseError(sub)
    }
}

#[derive(Debug)]
pub struct ParserErrorItem {
    pub input: String,
    pub error: winnow::error::ErrorKind,
}

fn get_line_from_offset(source_code: &str, offset:usize) -> &str{
        &source_code[source_code.find_line_range(offset)]
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Type miss match")]
    TypeError(#[from] types::TypeError),
    #[error("Class miss match")]
    ClassError(#[from] types::ClassError),
    #[error("IO Error")]
    IoError(#[from] std::io::Error),
    #[error("The assignt member has been redefined.")]
    RedefinedMember,
    #[error("Redefinition of an enum variants.")]
    RedefinedEnumVariants,
    #[error("Redefinition of an element variable.")]
    RedefinedElement,
    #[error("Use of an undefined element variable.")]
    UndefinedElement,
    #[error("A membervaribale was not defined.\n {missing_members:?}")]
    UndefinedMember {
        missing_members: Vec<(String, String)>,
    },
    #[error("This code shoudl be Unreachable. Reason:\n {0}")]
    Unreachable(&'static str),
    #[error("Strange module name.")]
    StrangeModule,
    #[error("Can not find module")]
    ModuleNotFound,
    #[error("Parsing error {context:?}\n\n\t {}", get_line_from_offset(.source_code, *.offset))
    ]
    ParseError{
        context: winnow::error::ContextError,
        offset: usize,
        source_code: String
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Str(Str),
    List(Vec<Value>),
    EnumValue(types::EnumRef, Str),
    Object(ValueTree),
}

impl Value {
    fn typed_object(datatype: types::ClassRef) -> Self {
        Value::Object(ValueTree::new(datatype))
    }
    fn bool(value: bool) -> Self {
        Value::Bool(value)
    }
    fn int(value: i64) -> Self {
        Value::Int(value)
    }
    fn str(value: &str) -> Self {
        Value::Str(value.into())
    }
    fn list(value: &[Constant]) -> Self {
        Value::List(value.iter().map(|c| Value::from(c)).collect())
    }
    fn enum_value(enum_type: types::EnumRef,value: &str) -> Self {
        Value::EnumValue(enum_type, value.into())
    }
    fn get_type(&self) -> types::Type {
        match self {
            Self::Bool(_) => types::Type::Bool,
            Self::Int(_) => types::Type::Int,
            Self::Str(_) => types::Type::Str,
            Self::List(values) => types::Type::List(UnionType::from_vec(values.iter().map(|v| v.get_type()).collect())),
            Self::EnumValue(enum_type, ..) => types::Type::Enum(enum_type.clone()),
            Self::Object(value_tree) => types::Type::Class(value_tree.datatype.clone()),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}
impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}
impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::Str(value.into())
    }
}
impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::Str(value.into())
    }
}
impl From<&Constant<'_>> for Value {
    fn from(item: &Constant) -> Self {
        match item {
            Constant::Bool(b) => Value::bool(*b),
            Constant::Str(v) => Value::str(v),
            Constant::Int(v) => Value::int(*v),
            Constant::List(v) => Value::list(&v),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueTree {
    tree: BTreeMap<Str, Value>,
    pub datatype: types::ClassRef,
}

impl ValueTree {
    fn new(datatype: types::ClassRef) -> Self {
        let tree = datatype.default_value();

        Self { tree, datatype }
    }

    fn insert(&mut self, key: &str, value: Value) -> Result<(), CompileError> {
        self.datatype.check_member_type(key, &value)?;
        if let Some(_) = self.tree.insert(key.into(), value) {
            //return Err(CompileError::RedefinedMember);
        }
        Ok(())
    }

    pub fn get(&self, path: &str) -> Option<&Value> {
        let mut tree = Some(self);
        let mut value = None;
        for sub_path in path[1..].split('.') {
            match tree?.tree.get(sub_path) {
                Some(v @ Value::Object(t)) => {
                    tree = Some(t);
                    value = Some(v);
                }
                Some(v) => {
                    value = Some(v);
                    tree = None;
                }
                None => {
                    return None;
                }
            }
        }
        value
    }
    pub fn check(&self) -> Result<(), CompileError> {
        let mut missing_members = self
            .datatype
            .members()
            .filter(|(member_name, _)| !self.tree.contains_key(*member_name))
            .peekable();
        if missing_members.peek().is_some() {
            let errors = missing_members
                .map(|(member_name, datatype)| {
                    (format!("{}", member_name), format!("{}", datatype))
                })
                .collect();
            return Err(CompileError::UndefinedMember {
                missing_members: errors,
            });
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Element(ValueTree);

impl Element {
    pub fn new(datatype: types::ClassRef) -> Self {
        Self(ValueTree::new(datatype))
    }

    fn insert(&mut self, key: &str, value: Value) -> Result<(), CompileError> {
        self.0.insert(key, value)
    }

    pub fn get(&self, path: &str) -> Option<&Value> {
        self.0.get(path)
    }
}

#[derive(Debug)]
pub struct Flow(ValueTree);

impl Flow {
    pub fn new(datatype: types::ClassRef) -> Self {
        Self(ValueTree::new(datatype))
    }
    fn insert(&mut self, key: &str, value: Value) -> Result<(), CompileError> {
        self.0.insert(key, value)
    }
    pub fn get(&self, path: &str) -> Option<&Value> {
        self.0.get(path)
    }
}
pub struct Finding {
    threat: (),
    nodes: Box<[NodeIndex]>,
}

#[derive(Debug)]
pub struct ModelCompiler {
    graph: Graph<Element, Flow>,
    types: types::TypeTable,
    element_map: BTreeMap<Str, NodeIndex>,
    path: Vec<PathBuf>,
}
impl ModelCompiler {
    pub fn new(path: Vec<PathBuf>) -> Self {
        Self {
            graph: Graph::new(),
            types: types::TypeTable::new(),
            element_map: BTreeMap::new(),
            path,
        }
    }
    fn get_type_from_name(&self,name: &str) -> Result<types::Type, CompileError> {
        match name {
            "Bool" => Ok(types::Type::Bool),
            "Int" => Ok(types::Type::Int),
            "String" => Ok(types::Type::Str),
            name => {
                Ok(self.types.get_type(name)?)
            }
        }
    }
    fn get_type(&self, datatype: &parser::UnionType) -> Result<types::UnionType, CompileError> {
        let inner_types:Result<Vec<_>, CompileError> = datatype.iter().map(|dt| {
            match dt {
                // Class, Enum, String, Int, Bool
                Datatype::SingleType(name) => {
                    self.get_type_from_name(name)
                },
                Datatype::List(list_type) => {
                    Ok(types::Type::List(self.get_type(list_type)?))
                }
            }
        }
        ).collect();
        Ok(types::UnionType::from_vec(inner_types?))
    }

    fn value(&self, value: &MemberValue) -> Result<Value, CompileError> {
        match value {
            MemberValue::Object { datatype, members } => {
                let datatype = self.types.get_class(datatype)?;
                let mut object = ValueTree::new(datatype.clone());
                for member in members {
                    let member_value = self.value(&member.value)?;
                    datatype.check_member_type(member.name, &member_value)?;
                    object.insert(member.name, member_value)?;
                }
                Ok(Value::Object(object))
            }
            MemberValue::Enum{datatype, value }=>{
                let datatype: Vec<String> = datatype.iter().map(|s|s.to_string()).collect();
                let datatype = datatype.join(".");
                let datatype = self.types.get_enum(&datatype)?;
                Ok(Value::enum_value(datatype.clone(), value))
            },
            MemberValue::Const(c) => Ok(c.into()),
        }
    }

    pub fn compile(&mut self, ast: &Vec<Node>) -> Result<(), CompileError> {
        for node in ast {
            match node {
                Node::Class {
                    name,
                    super_type,
                    members,
                    doc_str: _,
                } => {
                    let mut new_class = if let Some(super_type) = super_type {
                        let s = self.types.get_class(super_type)?;
                        types::Class::sub_class(name, s)
                    } else {
                        types::Class::new(name)
                    };
                    for m in members {
                        match m {
                            ClassMember::Declaration {
                                name,
                                datatype,
                                doc_str: _,
                            } => {
                                let datatype = self.get_type(datatype)?;
                                new_class.add_member(name, datatype)?;
                            }
                            ClassMember::Assignment {
                                name,
                                value,
                                doc_str: _,
                            } => {
                                let default_value = self.value(&value)?;
                                new_class.add_default_member(name, default_value)?;
                            }
                        }
                    }
                    self.types.add_class(name, new_class)?;
                }
                Node::Enum {
                    name,
                    members,
                    doc_str: _,
                } => {
                    let mut variants = BTreeSet::new();
                    for member in members {
                        if !variants.insert((member.name).into()) {
                            return Err(CompileError::RedefinedEnumVariants);
                        }
                    }
                    let new_enum = types::Enum::new(name, variants);
                    self.types.add_enum(name, new_enum)?;
                }
                Node::Element {
                    name,
                    datatype,
                    members,
                    doc_str: _,
                } => {
                    let classtype = self.types.get_class(datatype)?;
                    let mut element = Element::new(classtype.clone());
                    for member in members {
                        let member_value = self.value(&member.value)?;
                        element.insert(member.name, member_value)?;
                    }
                    element.0.check()?;
                    self.add_by_name(name, element)?;
                }
                Node::Flow {
                    from,
                    to,
                    datatype,
                    members,
                    doc_str,
                } => self.add_node_flow(from, to, datatype, members, doc_str)?,
                Node::Sequence {
                    name: _,
                    flows: _,
                    doc_str: _,
                } => {}
                Node::Module {
                    name: _,
                    members: _,
                    doc_str: _,
                } => {
                    todo!();
                }
                Node::Import { name } => {
                    let module_name = {
                        let mut path = PathBuf::from(".");
                        for n in name {
                            path.push(n);
                        }
                        path.set_extension(EXTENSION);
                        path
                    };
                    let mut candidates = self
                        .path
                        .iter()
                        .map(|p| p.join(&module_name))
                        .filter(|x| x.exists());
                    if let Some(candidate) = candidates.next() {
                        self.compile_file(&candidate)?;
                    } else {
                        return Err(CompileError::ModuleNotFound);
                    }
                }
                Node::Comment(_lines) => {
                }
            }
        }
        Ok(())
    }

    fn add_node_flow(
        &mut self,
        from: &str,
        to: &str,
        datatype: &str,
        members: &Vec<ObjectMember>,
        _doc_str: &Vec<&str>,
    ) -> Result<(), CompileError> {
        let from = self.get_element(from)?;
        let to = self.get_element(to)?;
        let classtype = self.types.get_class(datatype)?;
        let mut flow = Flow::new(classtype.clone());
        for member in members {
            let member_value = self.value(&member.value)?;
            flow.insert(member.name, member_value)?;
        }
        flow.0.check()?;

        self.connect(from, to, flow);
        Ok(())
    }

    fn add(&mut self, e: Element) -> NodeIndex {
        self.graph.add_node(e)
    }
    fn add_by_name(&mut self, name: &str, e: Element) -> Result<(), CompileError> {
        let entry = self.element_map.entry(name.into());
        match entry {
            Entry::Occupied(_) => {
                return Err(CompileError::RedefinedElement);
            }
            Entry::Vacant(vacant_entry) => {
                let idx = self.graph.add_node(e);
                vacant_entry.insert(idx);
            }
        }
        Ok(())
    }
    fn get_element(&self, name: &str) -> Result<NodeIndex, CompileError> {
        let idx = self
            .element_map
            .get(name)
            .ok_or(CompileError::UndefinedElement)?;
        Ok(*idx)
    }

    pub fn compile_file(&mut self, path: &Path) -> Result<(), CompileError> {
        let mut file = File::open(path)?;

        let mut model_str = String::new();
        file.read_to_string(&mut model_str)?;
        let ast = match parser::parse_model(&model_str){
            Ok(ok) => ok,
            Err(e) => {
                let e = CompileError::ParseError{
                    offset: e.offset(),
                    context:e.into_inner(),
                    source_code: model_str,
                };
                return Err(e);
            }
        };

        self.compile(&ast)?;
        Ok(())
    }

    pub fn connect(&mut self, a: NodeIndex, b: NodeIndex, flow: Flow) -> EdgeIndex {
        self.graph.add_edge(a, b, flow)
    }
    pub fn build(self) -> Model {
        let matrix = self.graph.adjacency_matrix();
        Model {
            graph: self.graph,
            adjance_matrix: matrix,
        }
    }
}

#[derive(Debug)]
pub struct Model {
    pub graph: Graph<Element, Flow>,
    adjance_matrix: FixedBitSet,
}

impl Model {
    pub fn query(&self, query: &QueryGraph) -> usize {
        let result = algo::subgraph_isomorphism(&query, &self);
        result.len()
    }
}

pub struct QueryGraph {
    pub graph: Graph<QElement, QFlow>,
    adjance_matrix: FixedBitSet,
}

pub struct QueryGraphBuilder {
    graph: Graph<QElement, QFlow>,
}
impl QueryGraphBuilder {
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
        }
    }
    pub fn add(&mut self, e: QElement) -> NodeIndex {
        self.graph.add_node(e)
    }

    pub fn connect(&mut self, a: NodeIndex, b: NodeIndex, flow: QFlow) -> EdgeIndex {
        self.graph.add_edge(a, b, flow)
    }
    pub fn build(self) -> QueryGraph {
        let matrix = self.graph.adjacency_matrix();
        QueryGraph {
            graph: self.graph,
            adjance_matrix: matrix,
        }
    }
}

pub struct QElement(interpreter::Query);

impl QElement {
    fn new(query: &str) -> Result<Self, RunError> {
        let query = parser::parse_query(query)?;
        Ok(Self(query))
    }
}
pub struct QFlow(interpreter::Query);
impl QFlow {
    pub fn new(query: &str) -> Result<Self, RunError> {
        let query = parser::parse_query(query)?;
        Ok(Self(query))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn create_model() -> Model {
        let source_code = 
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
            client = Client {
                bool = true,
                client = true,
                str = \"test\",
            }
            server = Server {
                bool = true,
                server = true,
            }
            type Dummy {}
            other = Dummy {}
            client -> server = Dummy{}
            server -> client = Dummy{}";
        let ast = parser::parse_model(
            source_code
        );
        let ast = match ast {
            Ok(ast) => ast,
            Err(e) => {
                dbg!(get_line_from_offset(source_code, e.offset()));
                panic!();
            }
        };
        
        let mut model = ModelCompiler::new(vec![]);
        model.compile(&ast).unwrap();
        model.build()
    }

    #[test]
    fn test_create_model() {
        create_model();
    }
    #[test]
    fn test_query_model() {
        let model = create_model();

        let mut query = QueryGraphBuilder::new();
        let client = QElement::new(".bool").unwrap();
        query.add(client);
        let query = query.build();
        assert_eq!(model.query(&query), 2);
    }
    #[test]
    fn test_not_query_model() {
        let model = create_model();

        let mut query = QueryGraphBuilder::new();
        query.add(QElement::new("not .bool").unwrap());
        let query = query.build();
        assert_eq!(model.query(&query), 0);
    }
    #[test]
    fn test_none_member_query_model() {
        let model = create_model();

        let mut query = QueryGraphBuilder::new();
        query.add(QElement::new(".not_a_member").unwrap());
        let query = query.build();
        assert_eq!(model.query(&query), 0);
    }
    #[test]
    fn test_or_query_model() {
        let model = create_model();

        let mut query = QueryGraphBuilder::new();
        query.add(QElement::new(" true == .client or .server").unwrap());
        let query = query.build();
        assert_eq!(model.query(&query), 2);
    }
    #[test]
    fn test_or_gt_query_model() {
        let model = create_model();

        let mut query = QueryGraphBuilder::new();
        query.add(QElement::new(" true >= .client or .server").unwrap());
        let query = query.build();
        assert_eq!(model.query(&query), 2);
    }
    #[test]
    fn test_true_query_model() {
        let model = create_model();
        let mut query = QueryGraphBuilder::new();
        query.add(QElement::new("true").unwrap());
        let query = query.build();
        assert_eq!(model.query(&query), 3);
    }
    #[test]
    fn test_flow() {
        let model = create_model();

        let mut query = QueryGraphBuilder::new();
        let client = query.add(QElement::new(".client and true == .bool").unwrap());
        let server = query.add(QElement::new(".server").unwrap());
        let _request_login = query.connect(client, server, QFlow::new("true").unwrap());

        let query = query.build();
        assert_eq!(model.query(&query), 1);
    }
}
