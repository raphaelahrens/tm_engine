
use winnow::{
    ascii:: space0,
    combinator::{
        alt, cut_err,  eof, fail, opt, preceded, repeat, separated, terminated
    },
    error::{ContextError, StrContext},
    prelude::*,
    seq,
    token::any,
};

use crate::parser::{
    Constant,
    Stream,
    comment_block,
    block,
    comment_line,
    assign,
    identifier,
    ws,
    multi_ws,
    import,
    value_constant,
    enum_value,
    types::{
        type_declartion,
        UnionType,
    }
};

/**
 * A [Flow] represents the parsing output of a flow in tml.
 *
 * ```tml
 * a -> b = SomeType{member: 5}
 * ```
 *
 * A [Flow] can either standalone ([Node::Flow]) or be part of a [Node::Sequence].
 */
#[derive(Debug, PartialEq)]
pub struct Flow<'input>{
    pub from: &'input str,
    pub to: &'input str,
    pub datatype: &'input str,
    pub members: Vec<ObjectMember<'input>>,
    pub doc_str: Vec<&'input str>,
}

/**
 * The AST of a model is build using [Node] instances.
 */
#[derive(Debug, PartialEq)]
pub enum Node<'input> {
    Class {
        name: &'input str,
        super_type: Option<&'input str>,
        members: Vec<ClassMember<'input>>,
        doc_str: Vec<&'input str>,
    },
    Enum {
        name: &'input str,
        members: Vec<EnumMember<'input>>,
        doc_str: Vec<&'input str>,
    },
    Module {
        name: &'input str,
        members: Vec<Node<'input>>,
        doc_str: Vec<&'input str>,
    },
    Import {
        name: Vec<&'input str>,
    },
    Element {
        name: &'input str,
        datatype: &'input str,
        members: Vec<ObjectMember<'input>>,
        doc_str: Vec<&'input str>,
    },
    Flow(Flow<'input>),
    Sequence {
        name: Option<&'input str>,
        flows: Vec<Flow<'input>>,
        doc_str: Vec<&'input str>,
    },
    Comment(Vec<&'input str>),
}

/**
 * A [Node::Class] can have zero or more members which are either
 *
 * - a declaration ([ClassMember::Declaration]), which just definces the member name and type, or
 * - an assignment ([ClassMember::Assignment]), which defines the name and a default value.
 */
#[derive(Debug, PartialEq)]
pub enum ClassMember<'input> {
    Declaration {
        name: &'input str,
        datatype: UnionType<'input>,
        doc_str: Vec<&'input str>,
    },
    Assignment {
        name: &'input str,
        value: MemberValue<'input>,
        doc_str: Vec<&'input str>,
    },
}

/**
 * An [ObjectMember] is the assignment of a class member.
 *
 * ```tml
 *    {a = "example"}
 * ```
 */
#[derive(Debug, PartialEq)]
pub struct ObjectMember<'input> {
    pub name: &'input str,
    pub value: MemberValue<'input>,
    pub doc_str: Vec<&'input str>,
}

/**
 * An [EnumMember] is the declaration of one value an enum
 *
 * ```tml
 *    # comment
 *    EnumValue
 * ```
 */
#[derive(Debug, PartialEq)]
pub struct EnumMember<'input> {
    pub name: &'input str,
    pub doc_str: Vec<&'input str>,
}

/**
 * An [MemberValue] is the value a class member.
 *
 * Either an object, an enum or a [super::Constant]
 *
 * ```tml
 *    # comment block
 *    "example"
 *    Enum.Value
 *    {}
 * ```
 */
#[derive(Debug, PartialEq)]
pub enum MemberValue<'input> {
    Object {
        datatype: &'input str,
        members: Vec<ObjectMember<'input>>,
    },
    Enum{
        datatype: Vec<&'input str>,
        value: &'input str
    },
    Const(Constant<'input>),
}

/**
 * parse a [Node::Comment] it can have mutiple lines
 */
fn comment_node<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    repeat(1.., comment_line).map(|comments | Node::Comment(comments)).parse_next(input)
}
/**
 * parse the name opf a class 
 */
fn class_name<'i>(input: &mut Stream<'i>) -> PResult<&'i str> {
    identifier
        .parse_next(input)
}

/**
 * parse a `':'` serounded by whitespace
 */
fn colon<'i>(input: &mut Stream<'i>) -> PResult<char> {
    multi_ws(':').parse_next(input)
}

/**
 * parse a `','` serounded by whitespace
 */
fn comma<'i>(input: &mut Stream<'i>) -> PResult<char> {
    multi_ws(',')
        .context(StrContext::Label("comma"))
        .parse_next(input)
}

/**
 * parse a [EnumMember]
 */
fn enum_member<'i>(input: &mut Stream<'i>) -> PResult<EnumMember<'i>> {
    seq! {
        EnumMember{
            doc_str:comment_block,
            name:preceded(space0, identifier),
        }
    }
    .parse_next(input)
}

/**
 * parse a [Node::Enum].
 *
 * ```tml
 * enum Name{
 *  A,
 *  B,
 * }
 * ```
 */
fn enum_type<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    seq!(
        comment_block,
        preceded(ws("enum"), cut_err(identifier)),
        cut_err(block(terminated(
            separated(1.., enum_member, comma),
            opt(comma)
        )))
        .context(StrContext::Label("enum block")),
    )
    .map(|(doc_str, name, members)| Node::Enum {
        name,
        members,
        doc_str,
    })
    .parse_next(input)
}

/**
 * parse a declaration of a class member
 *
 * ```tml
 *  # comment
 *  abc: Type
 * ```
 *
 */
fn member_declaration<'i>(input: &mut Stream<'i>) -> PResult<ClassMember<'i>> {
    (
        comment_block,
        preceded(space0, identifier),
        colon,
        type_declartion,
    )
        .context(StrContext::Label("member declaration"))
        .map(|(doc_str, name, _, datatype)| ClassMember::Declaration {
            name,
            datatype,
            doc_str,
        })
        .parse_next(input)
}

/**
 * parse a constant value, either, which can be assigned to an object member
 *
 *   * a simple constant
 *     * an integer, a string, aboolean
 *   * a constant object, or
 *   * a constant enum
 */
fn constant<'i>(input: &mut Stream<'i>) -> PResult<MemberValue<'i>> {
    alt((
        object_constant,
        enum_value.map(|e| MemberValue::Enum { datatype: e.datatype, value: e.value }),
        value_constant.map(|c| MemberValue::Const(c)),
    ))
    .parse_next(input)
}

/**
 * parse an assignment of a [constant] to a member of an object
 *
 * ```tml
 * a = <constant>
 * ```
 */
fn assignment<'i>(
    input: &mut Stream<'i>,
) -> PResult<(Vec<&'i str>, &'i str, char, MemberValue<'i>)> {
    (
        comment_block,
        preceded(space0, identifier),
        assign,
        constant,
    )
        .context(StrContext::Label("assignment"))
        .parse_next(input)
}

/**
 * parses an assigment and maps it to an ObjectMember
 */
fn object_assignment<'i>(input: &mut Stream<'i>) -> PResult<ObjectMember<'i>> {
    assignment
        .context(StrContext::Label("object_assignment"))
        .map(|(doc_str, name, _, value)| ObjectMember {
            name,
            value,
            doc_str,
        })
        .parse_next(input)
}

/**
 * Parse a constant object
 *
 * ```tml
 * #comment block
 * Datatype{
 *   a = 5
 * }
 * ```
 */
fn object_constant<'i>(input: &mut Stream<'i>) -> PResult<MemberValue<'i>> {
    (
        identifier,
        block(terminated(
            separated(0.., object_assignment, comma),
            opt(comma),
        )),
    )
        .map(|(datatype, members)| MemberValue::Object { datatype, members })
        .parse_next(input)
}

fn member_assignment<'i>(input: &mut Stream<'i>) -> PResult<ClassMember<'i>> {
    assignment
        .context(StrContext::Label("member_assignment"))
        .map(|(doc_str, name, _, value)| ClassMember::Assignment {
            name,
            value,
            doc_str,
        })
        .parse_next(input)
}

fn member_definition<'i>(input: &mut Stream<'i>) -> PResult<ClassMember<'i>> {
    alt((member_assignment, member_declaration)).parse_next(input)
}

fn class_type<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    (
        comment_block,
        preceded(ws("type"), class_name),
        opt(preceded(colon, class_name)),
        block(terminated(
            separated(0.., member_definition, comma),
            opt(comma),
        )),
    )
        .map(|(doc_str, name, super_type, members)| Node::Class {
            name,
            super_type,
            members,
            doc_str,
        })
        .parse_next(input)
}

fn module<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    let doc_str = comment_block.parse_next(input)?;
    let name = preceded(ws("module"), identifier).parse_next(input)?;
    let members = block(repeat(0.., multi_ws(module_declaration))).parse_next(input)?;
    Ok(Node::Module {
        name,
        members,
        doc_str,
    })
}

fn module_declaration<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    alt((import.map(|name| Node::Import { name })
         , enum_type, class_type, module, comment_node))
        .context(StrContext::Label("declarations"))
        .parse_next(input)
}

pub fn parse_schema<'i>(input: &mut Stream<'i>) -> PResult<Vec<Node<'i>>> {
    terminated(repeat(0.., multi_ws(module_declaration)), eof)
        .context(StrContext::Label("schema"))
        .parse_next(input)
}

// Element example
// server = Process{
//      os = "FreeBSD-14"
// }
fn element_assign<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    (
        comment_block,
        preceded(space0, identifier),
        assign,
        cut_err((
            identifier,
            block(terminated(
                separated(0.., object_assignment, comma),
                opt(comma),
            )),
        )),
    )
        .context(StrContext::Label("element assignment"))
        .map(|(doc_str, name, _, (datatype, members))| Node::Element {
            name,
            datatype,
            members,
            doc_str,
        })
        .parse_next(input)
}

// Flow example
// client -> server = Type {}
fn flow_assign<'i>(input: &mut Stream<'i>) -> PResult<Flow<'i>> {
    (
        comment_block,
        preceded(space0, identifier),
        ws("->"),
        cut_err((
            identifier,
            assign,
            identifier,
            block(terminated(
                separated(0.., object_assignment, comma),
                opt(comma),
            )),
        )),
    )
        .context(StrContext::Label("flow assignment"))
        .map(
            |(doc_str, from, _, (to, _, datatype, members))| Flow {
                from,
                to,
                datatype,
                members,
                doc_str,
            },
        )
        .parse_next(input)
}

fn sequence<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    (
        comment_block,
        preceded(space0, "seq").context(StrContext::Label("seq")),
        opt(ws(identifier)),
        block(repeat(0.., flow_assign)),
    )
        .context(StrContext::Label("sequence"))
        .map(|(doc_str, _, name, flows)| Node::Sequence {
            name,
            flows,
            doc_str,
        })
        .parse_next(input)
}

fn model_declaration<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    alt((
            module_declaration,
            element_assign,
            flow_assign.map(|f| Node::Flow(f)),
            sequence,
            preceded(any, cut_err(fail.context(StrContext::Label("Unrecognized symbol"))))
        ))
        .context(StrContext::Label("model_declaration"))
        .parse_next(input)
}

pub fn parse_model<'i>(input: &'i str) -> Result<Vec<Node<'i>>, winnow::error::ParseError<Stream, ContextError>> {
    let input = Stream::new(input);
    repeat(0.., multi_ws(model_declaration))
    .context(StrContext::Label("model"))
    .parse(input.into())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::types::Datatype;

    #[test]
    fn test_class() {
        let result = parse_schema(&mut Stream::new(
            "
                type File { bool:Bool, test:String, }",
        ))
        .unwrap();
        if let Node::Class {
            name,
            super_type,
            members,
            doc_str: _,
        } = &result[0]
        {
            assert_eq!(*name, "File");
            assert_eq!(*super_type, None);
            assert_eq!(
                members[0],
                ClassMember::Declaration {
                    name: "bool",
                    datatype: UnionType(vec![Datatype::SingleType("Bool")]),
                    doc_str: vec![]
                }
            );
            assert_eq!(
                members[1],
                ClassMember::Declaration {
                    name: "test",
                    datatype: UnionType(vec![Datatype::SingleType("String")]),
                    doc_str: vec![]
                }
            );
        }
    }
    #[test]
    fn test_class_assignment() {
        let result = parse_schema(&mut Stream::new(
            " type File { bool = true, test:String, }",
        ))
        .unwrap();
        if let Node::Class {
            name,
            super_type,
            members,
            doc_str: _,
        } = &result[0]
        {
            assert_eq!(*name, "File");
            assert_eq!(*super_type, None);
            assert_eq!(
                members[0],
                ClassMember::Assignment {
                    name: "bool",
                    value: MemberValue::Const(Constant::Bool(true)),
                    doc_str: vec![]
                }
            );
            assert_eq!(
                members[1],
                ClassMember::Declaration {
                    name: "test",
                    datatype: UnionType(vec![Datatype::SingleType("String")]),
                    doc_str: vec![]
                }
            );
        }
    }
    #[test]
    fn test_class_super() {
        let result = parse_schema(&mut Stream::new(" type File:Super { bool:Bool, }")).unwrap();
        if let Node::Class {
            name,
            super_type,
            members,
            doc_str: _,
        } = &result[0]
        {
            assert_eq!(*name, "File");
            assert_eq!(*super_type, Some("Super"));
            assert_eq!(
                members[0],
                ClassMember::Declaration {
                    name: "bool",
                    datatype: UnionType(vec![Datatype::SingleType("Bool")]),
                    doc_str: vec![]
                }
            );
        } else {
            assert!(false);
        }
    }
    #[test]
    fn test_enum() {
        let result = parse_schema(&mut Stream::new(" enum Level { Test, Lol }")).unwrap();
        if let Node::Enum {
            name,
            members,
            doc_str: _,
        } = &result[0]
        {
            assert_eq!(*name, "Level");
            assert_eq!(
                members[0],
                EnumMember {
                    name: "Test",
                    doc_str: vec![]
                }
            );
            assert_eq!(
                members[1],
                EnumMember {
                    name: "Lol",
                    doc_str: vec![]
                }
            );
        } else {
            assert!(false);
        }
    }
    #[test]
    fn test_module() {
        let result =
            parse_schema(&mut Stream::new(" module mqtt { enum Publish{Bla, }}")).unwrap();
        if let Node::Module { name, members, .. } = &result[0] {
            assert_eq!(*name, "mqtt");
            assert_eq!(
                members[0],
                Node::Enum {
                    name: "Publish",
                    members: vec![EnumMember {
                        name: "Bla",
                        doc_str: vec![]
                    }],
                    doc_str: vec![]
                }
            );
        } else {
            assert!(false);
        }
    }
    #[test]
    fn test_multiple_classes() {
        let test_str = "
# Example Data
# More comments
type Data {
    sensitivity: Int,
    format: Atom,
    target_value: Atom,
}

type Datastore {
    data: Data,
    
}

enum Severity { High, Low, }
enum TextFormat {
    ASCII,
    UTF8,
}
enum Value {
    High,
    Average,
    Low,
}

type Credential:Data {
}

module HTTP {
    enum Method {
        GET,
        POST,
        PUT,
        HEAD,
        DELETE,
        CONNECT,
        OPTIONS,
        TRACE,
        PATCH,
    }
    type Request {
        path: String,
        headers: Header,
        data: Data,
    }
    type Response {
        headers: Header,
        data: Data,
    }
}

module MQTT {
    # This module is a representaion of the MQTT protocol
    type Publish {
        # More comments
        # even here
        payload: Data,
        QoS: int,
        topic: String,
        #Test comment str
        publish = true,

    }
}
        ";
        let result = parse_schema(&mut Stream::new(test_str.into())).unwrap();
        if let Node::Class {
            name,
            super_type,
            members,
            ..
        } = &result[0]
        {
            assert_eq!(*name, "Data");
            assert_eq!(*super_type, None);
            assert_eq!(
                members[0],
                ClassMember::Declaration {
                    name: "sensitivity",
                    datatype: UnionType(vec![Datatype::SingleType("Int")]),
                    doc_str: vec![]
                }
            );
            assert_eq!(
                members[1],
                ClassMember::Declaration {
                    name: "format",
                    datatype: UnionType(vec![Datatype::SingleType("Atom")]),
                    doc_str: vec![]
                }
            );
        }
    }
}
