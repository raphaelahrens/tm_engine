/*
 * Query = _{ SOI ~ Expr ~ EOI }
 * Expr = { NotExpr | LogicExpr | Term }
 * Term = _{ Member | Const | "(" ~ Expr ~ ")" }
 * NotExpr = { "!" ~ Term }
 * LogicExpr = { CompExpr ~ (LogicOperator ~ CompExpr)* }
 * LogicOperator = { "and" | "or" }
 * CompExpr = { Term ~ (CompOperator ~ Term)? }
 * CompOperator = {  "<" | "<=" | "==" | "!=" | ">=" | ">"}
 * Member = ${ ("." ~ Identifier)+}
 * Identifier = @{ASCII_ALPHA ~ ASCII_ALPHANUMERIC*}
 * Const = _{ Int | QuotedAtom | QuotedString }
 * Int = @{  ASCII_DIGIT+ }
 * QuotedAtom = ${ "'" ~ Atom }
 * Atom = @{  ASCII_ALPHA ~ ASCII_ALPHANUMERIC*}
 * QuotedString = ${ "\"" ~ String ~ "\""}
 * String = @{  ASCII_ALPHANUMERIC* }
 * WHITESPACE = _{ " " | "\t" | "\n" }
*/

use std::fmt;
use std::ops::RangeInclusive;

use winnow::{
    ascii::{
        digit1, hex_digit1, line_ending, multispace0, multispace1, space0, space1, till_line_ending,
    },
    combinator::{
        alt, cut_err, delimited, eof, opt, preceded, repeat, separated, terminated, trace, Repeat,
    },
    error::{ParserError, StrContext},
    prelude::*,
    seq,
    token::{any, none_of, one_of, take_while},
    Located,
};

const ALPHA: (RangeInclusive<char>, RangeInclusive<char>) = ('a'..='z', 'A'..='Z');
const DIGIT: RangeInclusive<char> = '0'..='9';
//pub type Span<'input> = LocatedSpan<&'input str>;
pub type Stream<'i> = Located<&'i str>;

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.

fn ws<'i, F, O, E>(inner: F) -> impl Parser<Stream<'i>, O, E>
where
    E: ParserError<Stream<'i>>
        + for<'a> winnow::error::AddContext<winnow::Located<&'a str>, winnow::error::StrContext>,
    F: Parser<Stream<'i>, O, E>,
{
    delimited(
        space0.context(StrContext::Label("leading space")),
        inner.context(StrContext::Label("inner")),
        space0.context(StrContext::Label("trailing space")),
    )
}

fn multi_ws<'i, F, O, E>(inner: F) -> impl Parser<Stream<'i>, O, E>
where
    E: ParserError<Stream<'i>>
        + for<'a> winnow::error::AddContext<winnow::Located<&'a str>, winnow::error::StrContext>,
    F: Parser<Stream<'i>, O, E>,
{
    delimited(
        multispace0.context(StrContext::Label("leading space")),
        inner.context(StrContext::Label("inner")),
        multispace0.context(StrContext::Label("trailing space")),
    )
}

#[derive(Debug, PartialEq)]
pub enum Constant<'input> {
    Bool(bool),
    Str(&'input str),
    Int(i64),
    EnumValue(&'input str),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operation {
    Bool(bool),
    Str(Box<str>),
    Int(i64),
    EnumValue(Box<str>),
    Member { name: Box<str>, pos: usize },
    Not,
    Greater,
    GreaterEq,
    Equal,
    LesserEq,
    Lesser,
    NotEqual,
    And,
    Or,
}

impl Operation {
    fn short_symbol(&self) -> char {
        match self {
            Self::LesserEq => '≤',
            Self::Lesser => '<',
            Self::Equal => '=',
            Self::NotEqual => '≠',
            Self::Greater => '>',
            Self::GreaterEq => '≥',
            Self::Not => '!',
            Self::And => '∧',
            Self::Or => '∨',
            Self::Bool(false) => '0',
            Self::Bool(true) => '1',
            Self::Str(_) => '"',
            Self::Int(_) => '#',
            Self::EnumValue(_) => 'E',
            Self::Member { name, .. } => {
                let mut iter = name.chars().skip(1);
                match iter.next() {
                    None => '?',
                    Some(c) => c,
                }
            }
        }
    }
}

impl From<Constant<'_>> for Operation {
    fn from(item: Constant) -> Self {
        match item {
            Constant::Bool(b) => Operation::Bool(b),
            Constant::Str(v) => Operation::Str(v.into()),
            Constant::Int(v) => Operation::Int(v),
            Constant::EnumValue(v) => Operation::EnumValue(v.into()),
        }
    }
}
#[derive(PartialEq)]
pub struct Query {
    pub ops: Box<[Operation]>,
    pub false_jumps: Box<[usize]>,
    pub true_jumps: Box<[usize]>,
}

fn set_jump(ops: &[Operation], false_jumps: &mut Vec<usize>, index: usize, argn: usize) {
    match ops.get(index) {
        Some(Operation::Member { pos, .. }) => {
            false_jumps.insert(index, argn - pos);
        }
        Some(_other) => {}
        None => {} //ignore this
    }
}

impl Query {
    pub fn new(ops: Box<[Operation]>) -> Self {
        let mut false_jumps = vec![0; ops.len()];
        let mut true_jumps = vec![0; ops.len()];

        for (i, op) in ops.iter().enumerate() {
            match op {
                Operation::Greater
                | Operation::GreaterEq
                | Operation::Equal
                | Operation::LesserEq
                | Operation::Lesser
                | Operation::NotEqual => {
                    set_jump(&ops, &mut false_jumps, i - 1, 2);
                    set_jump(&ops, &mut false_jumps, i - 2, 2);
                }
                Operation::Not => {
                    set_jump(&ops, &mut false_jumps, i - 1, 1);
                }
                _ => {}
            }
        }

        for i in (0..false_jumps.len() - 1).rev() {
            let old = false_jumps[i];
            let next_jump = false_jumps[i + old];
            false_jumps[i] = old + next_jump;
        }
        Query {
            ops,
            false_jumps: false_jumps.into_boxed_slice(),
            true_jumps: true_jumps.into_boxed_slice(),
        }
    }
    pub fn iter(&self) -> QueryIter {
        QueryIter {
            query: self,
            next: 0,
        }
    }
}

impl fmt::Debug for Query {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ops: String = self.ops.iter().map(Operation::short_symbol).collect();
        let members: Vec<&Box<str>> = self
            .ops
            .iter()
            .filter_map(|op| {
                if let Operation::Member { name, .. } = op {
                    Some(name)
                } else {
                    None
                }
            })
            .collect();

        f.debug_struct("Query")
            .field("ops", &ops)
            .field("members", &members)
            .finish()
    }
}

pub struct QueryIter<'query> {
    query: &'query Query,
    next: usize,
}

impl<'query> Iterator for QueryIter<'query> {
    type Item = (&'query Operation, usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.next;

        let op = self.query.ops.get(current)?;
        let false_jump = self.query.false_jumps.get(current)?;
        let true_jump = self.query.true_jumps.get(current)?;

        self.next = current + 1;

        // Since there's no endpoint to a Fibonacci sequence, the `Iterator`
        // will never return `None`, and `Some` is always returned.
        Some((op, *false_jump, *true_jump))
    }
}
fn bool<'i>(input: &mut Stream<'i>) -> PResult<Constant<'i>> {
    let parse_true = "true".value(true);

    // This is a parser that returns `false` if it sees the string "false", and
    // an error otherwise
    let parse_false = "false".value(false);

    alt((parse_true, parse_false))
        .map(|b| Constant::Bool(b))
        .parse_next(input)
}

fn character<'i>(input: &mut Stream<'i>) -> PResult<char> {
    let c = none_of('\"').parse_next(input)?;
    if c == '\\' {
        alt((any.verify_map(|c| {
            Some(match c {
                '"' | '\\' | '/' => c,
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => return None,
            })
        }),))
        .parse_next(input)
    } else {
        Ok(c)
    }
}

fn string<'i>(input: &mut Stream<'i>) -> PResult<Constant<'i>> {
    let chars: Repeat<_, _, _, (), _> = repeat(0.., character);
    preceded(
        '\"',
        // `cut_err` transforms an `ErrMode::Backtrack(e)` to `ErrMode::Cut(e)`, signaling to
        // combinators like  `alt` that they should not try other parsers. We were in the
        // right branch (since we found the `"` character) but encountered an error when
        // parsing the string
        cut_err(terminated(chars.recognize(), '\"')),
    )
    // `context` lets you add a static string to errors to provide more information in the
    // error chain (to indicate which parser had an error)
    .map(|s| Constant::Str(s))
    .parse_next(input)
}

fn identifier<'i>(input: &mut Stream<'i>) -> PResult<&'i str> {
    (one_of(ALPHA), take_while(0.., (ALPHA, DIGIT, '_')))
        .recognize()
        .context(StrContext::Label("identifier"))
        .parse_next(input)
}

fn sub_identifier<'i, const N: usize>(input: &mut Stream<'i>) -> PResult<&'i str> {
    let prefix: Repeat<_, _, _, (), _> = repeat(N.., preceded(identifier, '.'));
    preceded(prefix, identifier).recognize().parse_next(input)
}

fn int<'i>(input: &mut Stream) -> PResult<Constant<'i>> {
    alt((
        preceded(
            "0x",
            hex_digit1.verify_map(|s: &str| i64::from_str_radix(&s, 16).ok()),
        ),
        (
            opt('-'),
            digit1.verify_map(|s: &str| i64::from_str_radix(&s, 10).ok()),
        )
            .map(|(sign, i)| if let Some(_) = sign { -i } else { i }),
    ))
    .map(Constant::Int)
    .parse_next(input)
}

fn enum_value<'i>(input: &mut Stream<'i>) -> PResult<Constant<'i>> {
    sub_identifier::<1>
        .with_span()
        .map(|(s, r)| Constant::EnumValue(s))
        .parse_next(input)
}

fn value_constant<'i>(input: &mut Stream<'i>) -> PResult<Constant<'i>> {
    alt((bool, int, string, enum_value)).parse_next(input)
}

fn member<'i, const N: usize>(input: &mut Stream) -> PResult<Operation> {
    repeat::<_, _, (), _, _>(1.., preceded('.', identifier))
        .recognize()
        .map(|s| Operation::Member {
            name: (*s).into(),
            pos: N,
        })
        .parse_next(input)
}

fn comp_op<'i>(input: &mut Stream) -> PResult<Operation> {
    alt((
        "==".value(Operation::Equal),
        "!=".value(Operation::NotEqual),
        ">=".value(Operation::GreaterEq),
        "<=".value(Operation::LesserEq),
        ">".value(Operation::Greater),
        "<".value(Operation::Lesser),
    ))
    .parse_next(input)
}

fn term<'i, const N: usize>(input: &mut Stream) -> PResult<Operation> {
    alt((member::<N>, value_constant.map(|c| c.into()))).parse_next(input)
}

fn not_expr<'i, const N: usize>(input: &mut Stream) -> PResult<Vec<Operation>> {
    alt((
        preceded(("not", space1), not_expr::<0>).map(|mut e| {
            e.push(Operation::Not);
            e
        }),
        term::<N>.map(|t| vec![t]),
    ))
    .parse_next(input)
}

fn parents_expr<'i>(input: &mut Stream) -> PResult<Vec<Operation>> {
    delimited(('(', space0), logic_or, (space0, ')'))
        .map(|e| e)
        .parse_next(input)
}

fn collapse_tree<'a>(
    mut a: Vec<Operation>,
    mut b: Vec<Operation>,
    op: Operation,
) -> Vec<Operation> {
    a.append(&mut b);
    a.push(op);
    a
}

fn comp_expr<'i>(input: &mut Stream) -> PResult<Vec<Operation>> {
    alt((
        (not_expr::<0>, space0, comp_op, space0, not_expr::<1>)
            .map(|(a, _, op, _, b)| collapse_tree(a, b, op)),
        not_expr::<0>,
        parents_expr,
    ))
    .parse_next(input)
}

fn logic_and<'i>(input: &mut Stream) -> PResult<Vec<Operation>> {
    alt((
        (comp_expr, multispace1, "and", multispace1, logic_and)
            .map(|(a, _, _, _, b)| collapse_tree(a, b, Operation::And)),
        comp_expr,
    ))
    .parse_next(input)
}

fn logic_or<'i>(input: &mut Stream) -> PResult<Vec<Operation>> {
    alt((
        (logic_and, multispace1, "or", multispace1, logic_or)
            .map(|(a, _, _, _, b)| collapse_tree(a, b, Operation::Or)),
        logic_and,
    ))
    .parse_next(input)
}

pub fn parse_query<'i>(input: &str) -> PResult<Query> {
    terminated(ws(logic_or), eof)
        .map(|v| Query::new(v.into_boxed_slice()))
        .parse_next(&mut Located::new(input))
}

#[derive(Debug, PartialEq)]
pub enum Node<'input> {
    Class {
        name: &'input str,
        super_type: Option<&'input str>,
        members: Vec<Member<'input>>,
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
        name: &'input str,
    },
    Element {
        name: &'input str,
        datatype: &'input str,
        members: Vec<ObjectMember<'input>>,
        doc_str: Vec<&'input str>,
    },
    Flow {
        from: &'input str,
        to: &'input str,
        datatype: &'input str,
        members: Vec<ObjectMember<'input>>,
        doc_str: Vec<&'input str>,
    },
    Sequence {
        name: Option<&'input str>,
        flows: Vec<Node<'input>>,
        doc_str: Vec<&'input str>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Member<'input> {
    Declaration {
        name: &'input str,
        datatype: &'input str,
        doc_str: Vec<&'input str>,
    },
    Assignment {
        name: &'input str,
        value: MemberValue<'input>,
        doc_str: Vec<&'input str>,
    },
}

#[derive(Debug, PartialEq)]
pub struct ObjectMember<'input> {
    pub name: &'input str,
    pub value: MemberValue<'input>,
    pub doc_str: Vec<&'input str>,
}

#[derive(Debug, PartialEq)]
pub struct EnumMember<'input> {
    pub name: &'input str,
    pub doc_str: Vec<&'input str>,
}

#[derive(Debug, PartialEq)]
pub enum MemberValue<'input> {
    Object {
        datatype: &'input str,
        members: Vec<ObjectMember<'input>>,
    },
    Const(Constant<'input>),
}

fn block<'i, F, O, E>(inner: F) -> impl Parser<Stream<'i>, O, E>
where
    E: ParserError<Stream<'i>>
        + for<'a> winnow::error::AddContext<winnow::Located<&'a str>, winnow::error::StrContext>,
    F: Parser<Stream<'i>, O, E>,
{
    delimited(
        multi_ws('{').context(StrContext::Label("{")),
        inner,
        multi_ws('}'),
    )
}

pub fn comment_line<'i>(input: &mut Stream<'i>) -> PResult<&'i str> {
    preceded(ws('#'), till_line_ending).parse_next(input)
}

pub fn comment_block<'i>(input: &mut Stream<'i>) -> PResult<Vec<&'i str>> {
    preceded(
        multispace0,
        repeat(0.., terminated(comment_line, line_ending)),
    )
    .context(StrContext::Label("comment block"))
    .parse_next(input)
}

fn assign<'i>(input: &mut Stream<'i>) -> PResult<char> {
    multi_ws('=').parse_next(input)
}

fn colon<'i>(input: &mut Stream<'i>) -> PResult<char> {
    multi_ws(':').parse_next(input)
}

fn comma<'i>(input: &mut Stream<'i>) -> PResult<char> {
    multi_ws(',')
        .context(StrContext::Label("comma"))
        .parse_next(input)
}

fn enum_member<'i>(input: &mut Stream<'i>) -> PResult<EnumMember<'i>> {
    seq! {
        EnumMember{
            doc_str:comment_block,
            name:preceded(space0, identifier),
        }
    }
    .parse_next(input)
}

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
fn member_declaration<'i>(input: &mut Stream<'i>) -> PResult<Member<'i>> {
    (
        comment_block,
        preceded(space0, identifier),
        colon,
        identifier,
    )
        .context(StrContext::Label("member declaration"))
        .map(|(doc_str, name, _, datatype)| Member::Declaration {
            name,
            datatype,
            doc_str,
        })
        .parse_next(input)
}

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

fn object_assignment<'i>(input: &mut Stream<'i>) -> PResult<ObjectMember<'i>> {
    assignment
        .map(|(doc_str, name, _, value)| ObjectMember {
            name,
            value,
            doc_str,
        })
        .parse_next(input)
}

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

fn constant<'i>(input: &mut Stream<'i>) -> PResult<MemberValue<'i>> {
    alt((
        object_constant,
        value_constant.map(|c| MemberValue::Const(c)),
    ))
    .parse_next(input)
}

fn member_assignment<'i>(input: &mut Stream<'i>) -> PResult<Member<'i>> {
    assignment
        .map(|(doc_str, name, _, value)| Member::Assignment {
            name,
            value,
            doc_str,
        })
        .parse_next(input)
}

fn member_definition<'i>(input: &mut Stream<'i>) -> PResult<Member<'i>> {
    alt((member_assignment, member_declaration)).parse_next(input)
}

fn class_type<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    (
        comment_block,
        preceded(ws("type"), identifier),
        opt(preceded(colon, identifier)),
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

fn import<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    preceded(ws("import"), sub_identifier::<0>)
        .map(|name| Node::Import { name })
        .parse_next(input)
}

fn module_declaration<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
    alt((import, enum_type, class_type, module))
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
fn flow_assign<'i>(input: &mut Stream<'i>) -> PResult<Node<'i>> {
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
            |(doc_str, from, _, (to, _, datatype, members))| Node::Flow {
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
        preceded(space0, "seq"),
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
    alt((module_declaration, element_assign, flow_assign, sequence))
        .context(StrContext::Label("model_declaration"))
        .parse_next(input)
}

pub fn parse_model<'i>(input: &'i str) -> PResult<Vec<Node<'i>>> {
    let input = Stream::new(input);
    trace(
        "model",
        terminated(repeat(0.., multi_ws(model_declaration)), eof),
    )
    .context(StrContext::Label("model"))
    .parse_next(&mut input.into())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_comment() {
        let result = comment_line(&mut Located::new(" # ")).unwrap();
        assert_eq!(result, "");
    }
    #[test]
    fn test_comment2() {
        let result = comment_line(&mut Located::new(" # lol  asdasd")).unwrap();
        assert_eq!(result, "lol  asdasd");
    }

    #[test]
    fn test_comment_single_block() {
        let result = comment_block(&mut Located::new("# lol222\n")).unwrap();
        assert_eq!(result, vec!["lol222"]);
    }

    #[test]
    fn test_comment_block() {
        let result = comment_block(&mut Located::new(
            "# lol
                # lol2\n",
        ))
        .unwrap();
        assert_eq!(result, vec!["lol", "lol2"]);
    }
    #[test]
    fn test_comment_block_lead_newline() {
        let result = comment_block(&mut Located::new(
            "

                # lol
                # lol2\n",
        ))
        .unwrap();
        assert_eq!(result, vec!["lol", "lol2"]);
    }

    #[test]
    fn test_class() {
        let result = parse_schema(&mut Located::new(
            "
                type File { bool:Bool, test:String, }",
        ))
        .unwrap();
        if let Node::Class {
            name,
            super_type,
            members,
            doc_str,
        } = &result[0]
        {
            assert_eq!(*name, "File");
            assert_eq!(*super_type, None);
            assert_eq!(
                members[0],
                Member::Declaration {
                    name: "bool",
                    datatype: "Bool",
                    doc_str: vec![]
                }
            );
            assert_eq!(
                members[1],
                Member::Declaration {
                    name: "test",
                    datatype: "String",
                    doc_str: vec![]
                }
            );
        }
    }
    #[test]
    fn test_class_assignment() {
        let result = parse_schema(&mut Located::new(
            " type File { bool = true, test:String, }",
        ))
        .unwrap();
        if let Node::Class {
            name,
            super_type,
            members,
            doc_str,
        } = &result[0]
        {
            assert_eq!(*name, "File");
            assert_eq!(*super_type, None);
            assert_eq!(
                members[0],
                Member::Assignment {
                    name: "bool",
                    value: MemberValue::Const(Constant::Bool(true)),
                    doc_str: vec![]
                }
            );
            assert_eq!(
                members[1],
                Member::Declaration {
                    name: "test",
                    datatype: "String",
                    doc_str: vec![]
                }
            );
        }
    }
    #[test]
    fn test_class_super() {
        let result = parse_schema(&mut Located::new(" type File:Super { bool:Bool, }")).unwrap();
        if let Node::Class {
            name,
            super_type,
            members,
            doc_str,
        } = &result[0]
        {
            assert_eq!(*name, "File");
            assert_eq!(*super_type, Some("Super"));
            assert_eq!(
                members[0],
                Member::Declaration {
                    name: "bool",
                    datatype: "Bool",
                    doc_str: vec![]
                }
            );
        } else {
            assert!(false);
        }
    }
    #[test]
    fn test_enum() {
        let result = parse_schema(&mut Located::new(" enum Level { Test, Lol }")).unwrap();
        if let Node::Enum {
            name,
            members,
            doc_str,
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
            parse_schema(&mut Located::new(" module mqtt { enum Publish{Bla, }}")).unwrap();
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
        let result = parse_schema(&mut Located::new(test_str.into())).unwrap();
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
                Member::Declaration {
                    name: "sensitivity",
                    datatype: "Int",
                    doc_str: vec![]
                }
            );
            assert_eq!(
                members[1],
                Member::Declaration {
                    name: "format",
                    datatype: "Atom",
                    doc_str: vec![]
                }
            );
        }
    }
    #[test]
    fn test_bool() {
        assert_eq!(
            bool(&mut Located::new("true")).unwrap(),
            Constant::Bool(true)
        );
        assert_eq!(
            bool(&mut Located::new("false")).unwrap(),
            Constant::Bool(false)
        );
    }
    #[test]
    fn test_string() {
        assert_eq!(
            string(&mut Located::new("\"\"")).unwrap(),
            Constant::Str("".into())
        );
        assert_eq!(
            string(&mut Located::new("\"test\"")).unwrap(),
            Constant::Str("test")
        );
    }
    #[test]
    fn test_int() {
        assert_eq!(int(&mut Located::new("15")).unwrap(), Constant::Int(15));
        assert_eq!(int(&mut Located::new("0x15")).unwrap(), Constant::Int(0x15));
        assert_eq!(int(&mut Located::new("0xFE")).unwrap(), Constant::Int(0xfe));
        assert_eq!(int(&mut Located::new("-15")).unwrap(), Constant::Int(-15));
        assert_eq!(int(&mut Located::new("0")).unwrap(), Constant::Int(0));
    }
    #[test]
    fn test_member() {
        assert_eq!(
            member::<0>(&mut Located::new(".a")).unwrap(),
            Operation::Member {
                name: ".a".into(),
                pos: 0
            }
        );
        assert_eq!(
            member::<0>(&mut Located::new(".a.b")).unwrap(),
            Operation::Member {
                name: ".a.b".into(),
                pos: 0
            }
        );
        assert_eq!(
            member::<0>(&mut Located::new(".a.b.xyz")).unwrap(),
            Operation::Member {
                name: ".a.b.xyz".into(),
                pos: 0
            }
        );
    }
    #[test]
    fn test_comp_op() {
        assert_eq!(comp_op(&mut Located::new("==")).unwrap(), Operation::Equal);
        assert_eq!(
            comp_op(&mut Located::new("!=")).unwrap(),
            Operation::NotEqual
        );
        assert_eq!(comp_op(&mut Located::new("<")).unwrap(), Operation::Lesser);
        assert_eq!(comp_op(&mut Located::new(">")).unwrap(), Operation::Greater);
        assert_eq!(
            comp_op(&mut Located::new("<=")).unwrap(),
            Operation::LesserEq
        );
        assert_eq!(
            comp_op(&mut Located::new(">=")).unwrap(),
            Operation::GreaterEq
        );
    }
    #[test]
    fn test_comp_expr() {
        assert_eq!(
            comp_expr(&mut Located::new(".a==.b")).unwrap(),
            vec![
                Operation::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Operation::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Operation::Equal
            ]
        );
        assert_eq!(
            comp_expr(&mut Located::new(".a!=5")).unwrap(),
            vec![
                Operation::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Operation::Int(5),
                Operation::NotEqual
            ]
        );
        assert_eq!(
            comp_expr(&mut Located::new(".abc<\"lol\"")).unwrap(),
            vec![
                Operation::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Operation::Str("lol".into()),
                Operation::Lesser
            ]
        );
        assert_eq!(
            comp_expr(&mut Located::new("4>.abc")).unwrap(),
            vec![
                Operation::Int(4),
                Operation::Member {
                    name: ".abc".into(),
                    pos: 1
                },
                Operation::Greater
            ]
        );
        assert_eq!(
            comp_expr(&mut Located::new("42 > .abc")).unwrap(),
            vec![
                Operation::Int(42),
                Operation::Member {
                    name: ".abc".into(),
                    pos: 1
                },
                Operation::Greater
            ]
        );
        assert_eq!(
            comp_expr(&mut Located::new("true")).unwrap(),
            vec![Operation::Bool(true)]
        );
        assert_eq!(
            comp_expr(&mut Located::new("5")).unwrap(),
            vec![Operation::Int(5)]
        );
    }
    #[test]
    fn test_not_expr() {
        assert_eq!(
            comp_expr(&mut Located::new("42 > not .abc")).unwrap(),
            vec![
                Operation::Int(42),
                Operation::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Operation::Not,
                Operation::Greater
            ]
        );
        assert_eq!(
            comp_expr(&mut Located::new("not true")).unwrap(),
            vec![Operation::Bool(true), Operation::Not]
        );
        assert_eq!(
            comp_expr(&mut Located::new("not 5")).unwrap(),
            vec![Operation::Int(5), Operation::Not]
        );
    }
    #[test]
    fn test_logic_expr() {
        assert_eq!(
            logic_and(&mut Located::new(".a==.b and .abc")).unwrap(),
            vec![
                Operation::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Operation::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Operation::Equal,
                Operation::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Operation::And
            ]
        );
        assert_eq!(
            logic_and(&mut Located::new(".a == .b    and    .abc")).unwrap(),
            vec![
                Operation::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Operation::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Operation::Equal,
                Operation::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Operation::And
            ]
        );
        assert_eq!(
            logic_and(&mut Located::new(".a == .b and .abc and .test")).unwrap(),
            vec![
                Operation::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Operation::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Operation::Equal,
                Operation::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Operation::Member {
                    name: ".test".into(),
                    pos: 0
                },
                Operation::And,
                Operation::And
            ]
        );
        assert_eq!(
            logic_or(&mut Located::new(".a == .b or .abc and .test")).unwrap(),
            vec![
                Operation::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Operation::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Operation::Equal,
                Operation::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Operation::Member {
                    name: ".test".into(),
                    pos: 0
                },
                Operation::And,
                Operation::Or
            ]
        );
        assert_eq!(
            logic_or(&mut Located::new("(.a == .b or .abc) and .test")).unwrap(),
            vec![
                Operation::Member {
                    name: ".a".into(),
                    pos: 0
                },
                Operation::Member {
                    name: ".b".into(),
                    pos: 1
                },
                Operation::Equal,
                Operation::Member {
                    name: ".abc".into(),
                    pos: 0
                },
                Operation::Or,
                Operation::Member {
                    name: ".test".into(),
                    pos: 0
                },
                Operation::And
            ]
        );
    }
    #[test]
    fn example_query() {
        assert_eq!(
            parse_query("  ( .a ==  .b   or   .abc )   and   not .test  ").unwrap(),
            Query::new(
                [
                    Operation::Member {
                        name: ".a".into(),
                        pos: 0
                    },
                    Operation::Member {
                        name: ".b".into(),
                        pos: 1
                    },
                    Operation::Equal,
                    Operation::Member {
                        name: ".abc".into(),
                        pos: 0
                    },
                    Operation::Or,
                    Operation::Member {
                        name: ".test".into(),
                        pos: 0
                    },
                    Operation::Not,
                    Operation::And
                ]
                .into()
            )
        );
    }
}
