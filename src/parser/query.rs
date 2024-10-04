use winnow::{
    ascii::{
        space0, space1,
    },
    combinator::{
        alt, cut_err, delimited, eof, fail, preceded, repeat, separated_pair, terminated,
    },
    error::{ContextError, StrContext},
    prelude::*,
    token::any,
};

use crate::{interpreter::{self, Operation}, types::{self, TypeTable}};

use super::{
    assign, comment_block, enum_value, identifier, import, multi_ws, multi_ws1, value_constant, ws, ws1, Constant, Enum, Stream
};

/**
 * [Op] is an AST represesantation of the a query operation. From an [Op] an
 * [interpreter::Operation] can be build.
 */
#[derive(Debug, PartialEq)]
enum Op<'input>{
    Bool(bool),
    Str(&'input str),
    Int(i64),
    Enum(Enum<'input>),
    Member { name: &'input str, pos: usize },
    Not,
    Greater,
    GreaterEq,
    Equal,
    LesserEq,
    Lesser,
    NotEqual,
    And,
    Or,
    In,
}

impl <'i> From<Constant<'i>> for Op<'i> {
    fn from(item: Constant<'i>) -> Self {
        match item {
            Constant::Bool(b) => Op::Bool(b),
            Constant::Str(v) => Op::Str(v),
            Constant::Int(v) => Op::Int(v),
            Constant::List(_v) => todo!(),
        }
    }
}

impl <'i> From<Enum<'i>> for Op<'i> {
    fn from(e: Enum<'i>) -> Self {
        Op::Enum(e)
    }
}

/**
 * A Query is a list of query operations, which can be compiled to a [interpreter::Query].
 */
#[derive(Debug, PartialEq)]
pub struct Query<'input>(Vec<Op<'input>>);

/**
 * A node of the query AST.
 *
 * Each node is either a query for an Element ([QNode::Element]), a flow from one element to
 * another ([QNode::Flow]), or an import ([QNode::Import])
 */
pub enum QNode<'input>{
    Element{
        name: &'input str,
        datatype: &'input str,
        query: Query<'input>,
        doc_str: Vec<&'input str>,
    },
    Flow{
        from: &'input str,
        to: &'input str,
        datatype: &'input str,
        query: Query<'input>,
        doc_str: Vec<&'input str>,
    },
    Import{
        name: Vec<&'input str>,
    }
}

/**
 * take one operator and two arguments and fold them into the list of the first argument `a`.
 *
 * a call to `collapse_tree(a,b,op)` is equal to `a||b||[op]`.
 */
fn collapse_tree<'i>(
    mut a: Vec<Op<'i>>,
    mut b: Vec<Op<'i>>,
    op: Op<'i>,
) -> Vec<Op<'i>> {
    a.append(&mut b);
    a.push(op);
    a
}

/**
 * Parse an assignment in a threat query.
 *
 * Example:
 * ```tql
 * var = Type(.query == 5)
 * ```
 */
fn query_assign<'i>(input: &mut Stream<'i>) -> PResult<QNode<'i>> {
    (
        comment_block,
        preceded(space0, identifier),
        assign,
        cut_err((
            identifier,
            query,
        )),
    )
        .context(StrContext::Label("element assignment"))
        .map(|(doc_str, name, _, (datatype, query))| QNode::Element {
            name,
            datatype,
            query,
            doc_str,
        })
        .parse_next(input)
}

/**
 * Parse a flow assignment in a threat query.
 *
 * Example:
 * ```tql
 * a -> b = Type(.flow = "some query")
 * ```
 */
fn qflow_assign<'i>(input: &mut Stream<'i>) -> PResult<QNode<'i>> {
    (
        comment_block,
        preceded(space0, identifier),
        ws("->"),
        cut_err((
            identifier,
            assign,
            identifier,
            query
        )),
    )
        .context(StrContext::Label("flow assignment"))
        .map(
            |(doc_str, from, _, (to, _, datatype, query))| QNode::Flow {
                from,
                to,
                datatype,
                query,
                doc_str,
            },
        )
        .parse_next(input)
}

/**
 * Parses eitehr an import, element assignment or flow assignment
 */
fn query_declaration<'i>(input: &mut Stream<'i>) -> PResult<QNode<'i>> {
    alt((
            import.map(|name| QNode::Import { name }),
            //comment_node,
            query_assign,
            qflow_assign,
            //sequence,
            preceded(any, cut_err(fail.context(StrContext::Label("Unrecognized symbol"))))
        ))
        .context(StrContext::Label("query_declaration"))
        .parse_next(input)
}

/**
 * Parse a query in tql
 *
 * Example:
 * ```tql
 * a = A_Type(.a == 5)
 * var = B_Type(.b == 5)
 * a -> b = Flow(.a_b = "some query")
 * ```
 */
pub fn parse_tql<'i>(input: &'i str) -> Result<Vec<QNode<'i>>, winnow::error::ParseError<Stream, ContextError>> {
    let input = Stream::new(input);
    repeat(0.., multi_ws(query_declaration))
        .context(StrContext::Label("model"))
        .parse(input.into())
}

/**
 * Parse just a query. (`.a == 5`)
 *
 * This is just for testing the interpreter and 
 */
pub fn parse_query<'i>(input: &'i str) -> Result<Query<'i>, winnow::error::ParseError<Stream, ContextError>> {
    let input = Stream::new(input);
    terminated(ws(logic_or), eof)
        .map(|v| Query(v))
        .parse(input.into())
}

/**
 * Parse a member accessor.
 *
 * `.member_name`
 */
fn member<'i, const N: usize>(input: &mut Stream<'i>) -> PResult<Vec<Op<'i>>> {
    repeat::<_, _, (), _, _>(1.., preceded('.', identifier))
        .recognize()
        .map(|s| vec![Op::Member {
            name: s,
            pos: N,
        }])
        .parse_next(input)
}

/*
 * Parse a compare operator
 */
fn comp_op<'i>(input: &mut Stream) -> PResult<Op<'i>> {
    alt((
        "==".map(|_|Op::Equal),
        "!=".map(|_|Op::NotEqual),
        ">=".map(|_|Op::GreaterEq),
        "<=".map(|_|Op::LesserEq),
        ">".map(|_|Op::Greater),
        "<".map(|_|Op::Lesser),
    ))
    .parse_next(input)
}

/**
 * Parse a Term either a member accessor, a constant or an enum value.
 */
fn term<'i, const N: usize>(input: &mut Stream<'i>) -> PResult<Vec<Op<'i>>> {
    alt((
            member::<N>,
            value_constant.map(|c| vec![c.into()]),
            enum_value.map(|e| vec![e.into()]),
            )).parse_next(input)
}

/**
 * Parse the in operator expression.
 */
fn in_expr<'i, const N: usize>(input: & mut Stream<'i>) -> PResult<Vec<Op<'i>>> {
    alt((
            separated_pair(term::<0>, ws1("in"), member::<1>).map(|(a,b)|collapse_tree(a, b, Op::In)
         ),
            term::<N>,
            ))
    .parse_next(input)
}

/**
 * Parse the not operator expression.
 */
fn not_expr<'i, const N: usize>(input: &mut Stream<'i>) -> PResult<Vec<Op<'i>>> {
    alt((
        preceded(("not", space1), not_expr::<0>).map(|mut e| {
            e.push(Op::Not);
            e
        }),
        in_expr::<N>,
    ))
    .parse_next(input)
}

/**
 * Parse an expression in parentheses.
 */
fn parents_expr<'i>(input: &mut Stream<'i>) -> PResult<Vec<Op<'i>>> {
    delimited(('(', space0), logic_or, (space0, ')'))
        .map(|e| e)
        .parse_next(input)
}

/**
 * Parse a compare operation.
 */
fn comp_expr<'i>(input: &mut Stream<'i>) -> PResult<Vec<Op<'i>>> {
    alt((
        (not_expr::<0>, ws(comp_op), not_expr::<1>)
            .map(|(a, op, b)| collapse_tree(a, b, op)),
        not_expr::<0>,
        parents_expr,
    ))
    .parse_next(input)
}

/**
 * Parse a logical and operation.
 */
fn logic_and<'i>(input: &mut Stream<'i>) -> PResult<Vec<Op<'i>>> {
    alt((
        separated_pair(comp_expr, multi_ws1("and"), logic_and)
            .map(|(a, b)| collapse_tree(a, b, Op::And)),
        comp_expr,
    ))
    .parse_next(input)
}

/**
 * Parse a logical or operation.
 */
fn logic_or<'i>(input: &mut Stream<'i>) -> PResult<Vec<Op<'i>>> {
    alt((
        separated_pair(logic_and, multi_ws1("or"), logic_or)
            .map(|(a, b)| collapse_tree(a, b, Op::Or)),
        logic_and,
    ))
    .parse_next(input)
}

/**
 * Parse a full query in parentheses.
 */
fn query<'i>(input: &mut Stream<'i>) -> PResult<Query<'i>> {
    delimited('(', ws(logic_or), ')')
        .map(|v| Query(v))
        .parse_next(input)
}

/**
 * Build an [`interpreter::Query`] from [`Query`]. 
 */
pub fn compile_query(query_tokens: Query, types: &TypeTable) -> Result<interpreter::Query, types::TypeError>{
    let ops: Result<Vec<interpreter::Operation>, types::TypeError> = query_tokens.0.iter().map(|t|{
        let op = match t {
            Op::Bool(b) => Operation::Bool(*b),
            Op::Str(s) => Operation::Str((*s).into()),
            Op::Int(i) => Operation::Int(*i),
            Op::Enum(e) => {
                let enum_type = types.get_enum(&e.datatype.join("."))?;
                Operation::EnumValue( enum_type.clone(), e.value.into())
            },
            Op::Member{ name, pos } => Operation::Member { name:(*name).into(), pos: *pos },
            Op::Not => Operation::Not,
            Op::Greater => Operation::Greater,
            Op::GreaterEq => Operation::GreaterEq,
            Op::Equal => Operation::Equal,
            Op::LesserEq => Operation::LesserEq,
            Op::Lesser => Operation::Lesser,
            Op::NotEqual => Operation::NotEqual,
            Op::And => Operation::And,
            Op::Or => Operation::Or,
            Op::In => Operation::In,
        };
        Ok(op)
    }).collect();
    Ok(interpreter::Query::new(ops?.into_boxed_slice()))
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_member() {
        assert_eq!(
            member::<0>(&mut Stream::new(".a")).unwrap(),
            vec![
            Op::Member {
                name: ".a".into(),
                pos: 0
            }
            ]
        );
        assert_eq!(
            member::<0>(&mut Stream::new(".a.b")).unwrap(),
            vec![
            Op::Member {
                name: ".a.b".into(),
                pos: 0
            }
            ]
        );
        assert_eq!(
            member::<0>(&mut Stream::new(".a.b.xyz")).unwrap(),
            vec![
            Op::Member {
                name: ".a.b.xyz".into(),
                pos: 0
            }
            ]
        );
    }
    #[test]
    fn test_comp_op() {
        assert_eq!(comp_op(&mut Stream::new("==")).unwrap(), Op::Equal);
        assert_eq!(
            comp_op(&mut Stream::new("!=")).unwrap(),
            Op::NotEqual
        );
        assert_eq!(comp_op(&mut Stream::new("<")).unwrap(), Op::Lesser);
        assert_eq!(comp_op(&mut Stream::new(">")).unwrap(), Op::Greater);
        assert_eq!(
            comp_op(&mut Stream::new("<=")).unwrap(),
            Op::LesserEq
        );
        assert_eq!(
            comp_op(&mut Stream::new(">=")).unwrap(),
            Op::GreaterEq
        );
    }
    #[test]
    fn test_comp_expr() {
        assert_eq!(
            comp_expr(&mut Stream::new(".a==.b")).unwrap(),
            vec![
                Op::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Op::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Op::Equal
            ]
        );
        assert_eq!(
            comp_expr(&mut Stream::new(".a!=5")).unwrap(),
            vec![
                Op::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Op::Int(5),
                Op::NotEqual
            ]
        );
        assert_eq!(
            comp_expr(&mut Stream::new(".abc<\"lol\"")).unwrap(),
            vec![
                Op::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Op::Str("lol".into()),
                Op::Lesser
            ]
        );
        assert_eq!(
            comp_expr(&mut Stream::new("4>.abc")).unwrap(),
            vec![
                Op::Int(4),
                Op::Member {
                    name: ".abc".into(),
                    pos: 1
                },
                Op::Greater
            ]
        );
        assert_eq!(
            comp_expr(&mut Stream::new("42 > .abc")).unwrap(),
            vec![
                Op::Int(42),
                Op::Member {
                    name: ".abc".into(),
                    pos: 1
                },
                Op::Greater
            ]
        );
        assert_eq!(
            comp_expr(&mut Stream::new("true")).unwrap(),
            vec![Op::Bool(true)]
        );
        assert_eq!(
            comp_expr(&mut Stream::new("5")).unwrap(),
            vec![Op::Int(5)]
        );
    }
    #[test]
    fn test_not_expr() {
        assert_eq!(
            comp_expr(&mut Stream::new("42 > not .abc")).unwrap(),
            vec![
                Op::Int(42),
                Op::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Op::Not,
                Op::Greater
            ]
        );
        assert_eq!(
            comp_expr(&mut Stream::new("not true")).unwrap(),
            vec![Op::Bool(true), Op::Not]
        );
        assert_eq!(
            comp_expr(&mut Stream::new("not 5")).unwrap(),
            vec![Op::Int(5), Op::Not]
        );
    }
    #[test]
    fn test_logic_expr() {
        assert_eq!(
            logic_and(&mut Stream::new(".a==.b and .abc")).unwrap(),
            vec![
                Op::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Op::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Op::Equal,
                Op::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Op::And
            ]
        );
        assert_eq!(
            logic_and(&mut Stream::new(".a == .b    and    .abc")).unwrap(),
            vec![
                Op::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Op::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Op::Equal,
                Op::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Op::And
            ]
        );
        assert_eq!(
            logic_and(&mut Stream::new(".a == .b and .abc and .test")).unwrap(),
            vec![
                Op::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Op::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Op::Equal,
                Op::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Op::Member {
                    name: ".test".into(),
                    pos: 0
                },
                Op::And,
                Op::And
            ]
        );
        assert_eq!(
            logic_or(&mut Stream::new(".a == .b or .abc and .test")).unwrap(),
            vec![
                Op::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Op::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Op::Equal,
                Op::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Op::Member {
                    name: ".test".into(),
                    pos: 0
                },
                Op::And,
                Op::Or
            ]
        );
        assert_eq!(
            logic_or(&mut Stream::new("(.a == .b or .abc) and .test")).unwrap(),
            vec![
                Op::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Op::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Op::Equal,
                Op::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Op::Or,
                Op::Member {
                    name: ".test".into(),
                    pos: 0
                },
                Op::And
            ]
        );
    }
    #[test]
    fn example_query() {
        assert_eq!(
            parse_query("  ( .a ==  .b   or   .abc )   and   not .test  ").unwrap(),
            Query(
                [
                    Op::Member {
                        name: ".a".into(),
                        pos: 0
                    },
                    Op::Member {
                        name: ".b".into(),
                        pos: 1
                    },
                    Op::Equal,
                    Op::Member {
                        name: ".abc".into(),
                        pos: 0
                    },
                    Op::Or,
                    Op::Member {
                        name: ".test".into(),
                        pos: 0
                    },
                    Op::Not,
                    Op::And
                ]
                .into()
            )
        );
    }
}
