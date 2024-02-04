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

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        alpha1, alphanumeric0, alphanumeric1, char, hex_digit1, i64, multispace0, multispace1,
        newline, not_line_ending, space0, space1,
    },
    combinator::{eof, into, map, map_res, opt, recognize, value},
    error::{ParseError, VerboseError},
    multi::{many0, many0_count, many1_count, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult,
};
use std::fmt;

//pub type Span<'input> = LocatedSpan<&'input str>;
pub type Span<'input> = &'input str;

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'input, F: 'input, O, E: ParseError<Span<'input>>>(
    inner: F,
) -> impl FnMut(Span<'input>) -> IResult<Span<'input>, O, E>
where
    F: Fn(Span<'input>) -> IResult<Span<'input>, O, E>,
{
    delimited(space0, inner, space0)
}

fn multi_ws<'input, F: 'input, O, E: ParseError<Span<'input>>>(
    inner: F,
) -> impl FnMut(Span<'input>) -> IResult<Span<'input>, O, E>
where
    F: Fn(Span<'input>) -> IResult<Span<'input>, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn ws_char<'input, E: ParseError<Span<'input>> + 'input>(
    c: char,
) -> impl FnMut(Span<'input>) -> IResult<Span<'input>, char, E>
where
{
    multi_ws(char(c))
}

#[derive(Debug, PartialEq)]
pub enum Constant<'input> {
    Bool(bool),
    Str(&'input str),
    Int(i64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operation {
    Bool(bool),
    Str(Box<str>),
    Int(i64),
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

fn bool(input: Span) -> IResult<Span, Constant, VerboseError<Span>> {
    alt((
        map(tag("true"), |_| Constant::Bool(true)),
        map(tag("false"), |_| Constant::Bool(false)),
    ))(input)
}

fn quote(input: Span) -> IResult<Span, char, VerboseError<Span>> {
    char('"')(input)
}

fn string(input: Span) -> IResult<Span, Constant, VerboseError<Span>> {
    //TODO this does not work with complex text (unicode)
    map(delimited(quote, alphanumeric0, quote), |s| {
        Constant::Str(&s)
    })(input)
}

fn sub_identifier(input: Span) -> IResult<Span, Span, VerboseError<Span>> {
        recognize(preceded(
            many0_count(preceded(identifier, char('.'))),
            identifier,
        ))(input)
}

//fn atom(input: Span) -> IResult<Span, Constant, VerboseError<Span>> {
//    map(
//        sub_identifier,
//        |s| Constant::Atom(&s),
//    )(input)
//}

fn int(input: Span) -> IResult<Span, Constant, VerboseError<Span>> {
    let hex_num = map_res(preceded(tag("0x"), hex_digit1), |s: Span| {
        i64::from_str_radix(&s, 16)
    });
    let dec_num = map(tuple((opt(tag("-")), i64)), |(sign, num)| match sign {
        None => num,
        Some(_) => -num,
    });
    map(alt((hex_num, dec_num)), Constant::Int)(input)
}

fn value_constant(input: Span) -> IResult<Span, Constant, VerboseError<Span>> {
    alt((bool, int, string))(input)
}

fn identifier(input: Span) -> IResult<Span, &str, VerboseError<Span>> {
    map(
        recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_")))))),
        |s: Span| s,
    )(input)
}

fn member<const N: usize>(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    let attribute = preceded(char('.'), identifier);
    map(recognize(many1_count(attribute)), |s| Operation::Member {
        name: (*s).into(),
        pos: N,
    })(input)
}

fn equal(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    value(Operation::Equal, tag("=="))(input)
}
fn not_equal(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    value(Operation::NotEqual, tag("!="))(input)
}
fn greater(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    value(Operation::Greater, tag(">"))(input)
}
fn greater_eq(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    value(Operation::GreaterEq, tag(">="))(input)
}
fn lesser(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    value(Operation::Lesser, tag("<"))(input)
}
fn lesser_eq(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    value(Operation::LesserEq, tag("<="))(input)
}

fn comp_op(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    alt((equal, not_equal, greater_eq, lesser_eq, greater, lesser))(input)
}

fn term<const N: usize>(input: Span) -> IResult<Span, Operation, VerboseError<Span>> {
    alt((member::<N>, into(value_constant)))(input)
}

fn not_expr<const N: usize>(input: Span) -> IResult<Span, Vec<Operation>, VerboseError<Span>> {
    alt((
        map(
            preceded(tuple((tag("not"), space1)), not_expr::<0>),
            |mut e| {
                e.push(Operation::Not);
                e
            },
        ),
        map(term::<N>, |t| vec![t]),
    ))(input)
}

fn parents_expr(input: Span) -> IResult<Span, Vec<Operation>, VerboseError<Span>> {
    map(
        delimited(
            tuple((tag("("), space0)),
            logic_or,
            tuple((space0, tag(")"))),
        ),
        |e| e,
    )(input)
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

fn comp_expr(input: Span) -> IResult<Span, Vec<Operation>, VerboseError<Span>> {
    alt((
        map(
            tuple((not_expr::<0>, space0, comp_op, space0, not_expr::<1>)),
            |(a, _, op, _, b)| collapse_tree(a, b, op),
        ),
        not_expr::<0>,
        parents_expr,
    ))(input)
}

fn logic_and(input: Span) -> IResult<Span, Vec<Operation>, VerboseError<Span>> {
    alt((
        map(
            tuple((comp_expr, multispace1, tag("and"), multispace1, logic_and)),
            |(a, _, _, _, b)| collapse_tree(a, b, Operation::And),
        ),
        map(comp_expr, |t| t),
    ))(input)
}

fn logic_or(input: Span) -> IResult<Span, Vec<Operation>, VerboseError<Span>> {
    alt((
        map(
            tuple((logic_and, multispace1, tag("or"), multispace1, logic_or)),
            |(a, _, _, _, b)| collapse_tree(a, b, Operation::Or),
        ),
        logic_and,
    ))(input)
}

pub fn parse_query(input: &str) -> IResult<Span, Query, VerboseError<Span>> {
    map(terminated(ws(logic_or), eof), |v| {
        Query::new(v.into_boxed_slice())
    })(input.into())
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
        members: Vec<&'input str>,
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
    Sequence{
        name: Option<&'input str>,
        flows: Vec<Node<'input>>,
        doc_str: Vec<&'input str>,
    }
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
pub enum MemberValue<'input> {
    Object {
        datatype: &'input str,
        members: Vec<ObjectMember<'input>>,
    },
    Const(Constant<'input>),
}

fn block<'input, F: 'input, O, E: ParseError<Span<'input>> + 'input>(
    inner: F,
) -> impl FnMut(Span<'input>) -> IResult<Span<'input>, O, E>
where
    F: FnMut(Span<'input>) -> IResult<Span<'input>, O, E>,
{
    delimited(multi_ws(char('{')), inner, multi_ws(char('}')))
}

pub fn comment_line<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, &'input str, VerboseError<Span>> {
    let (input, _c) = ws(char('#'))(input)?;
    let (rest, comment_str) = recognize(not_line_ending)(input)?;
    Ok((rest, &comment_str))
}

pub fn comment_block<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, Vec<&'input str>, VerboseError<Span>> {
    preceded(multispace0, many0(terminated(comment_line, newline)))(input)
}

fn assign<'input>(input: Span<'input>) -> IResult<Span<'input>, char, VerboseError<Span>> {
    ws(char('='))(input)
}

fn colon<'input>(input: Span<'input>) -> IResult<Span<'input>, char, VerboseError<Span>> {
    ws(char(':'))(input)
}

fn enum_member(input: Span) -> IResult<Span, &str, VerboseError<Span>> {
    identifier(input)
}

fn comma(input: Span) -> IResult<Span, char, VerboseError<Span>> {
    ws_char(',')(input)
}

fn enum_type(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    map(
        tuple((
            comment_block,
            preceded(ws(tag("enum")), identifier),
            block(terminated(separated_list1(comma, enum_member), opt(comma))),
        )),
        |(doc_str, name, members)| Node::Enum {
            name,
            members,
            doc_str,
        },
    )(input)
}
fn member_declaration<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, Member, VerboseError<Span>> {
    map(
        tuple((
            comment_block,
            preceded(space0, identifier),
            colon,
            identifier,
        )),
        |(doc_str, name, _, datatype)| Member::Declaration {
            name,
            datatype,
            doc_str,
        },
    )(input)
}

fn assignment<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, (Vec<&str>, &str, char, MemberValue), VerboseError<Span>> {
    tuple((
        comment_block,
        preceded(space0, identifier),
        assign,
        constant,
    ))(input)
}

fn object_assignment<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, ObjectMember, VerboseError<Span>> {
    map(assignment, |(doc_str, name, _, value)| ObjectMember {
        name,
        value,
        doc_str,
    })(input)
}

fn object_constant<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, MemberValue<'input>, VerboseError<Span>> {
    map(
        tuple((
            identifier,
            block(terminated(
                separated_list0(comma, object_assignment),
                opt(comma),
            )),
        )),
        |(datatype, members)| MemberValue::Object { datatype, members },
    )(input)
}

fn constant<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, MemberValue<'input>, VerboseError<Span>> {
    alt((
        object_constant,
        map(value_constant, |c| MemberValue::Const(c)),
    ))(input)
}

fn member_assignment<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, Member, VerboseError<Span>> {
    map(assignment, |(doc_str, name, _, value)| Member::Assignment {
        name,
        value,
        doc_str,
    })(input)
}

fn member_definition<'input>(
    input: Span<'input>,
) -> IResult<Span<'input>, Member, VerboseError<Span>> {
    alt((member_assignment, member_declaration))(input)
}

fn class_type(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    map(
        tuple((
            comment_block,
            preceded(ws(tag("type")), identifier),
            opt(preceded(colon, identifier)),
            block(terminated(
                separated_list0(comma, member_definition),
                opt(comma),
            )),
        )),
        |(doc_str, name, super_type, members)| {
            Node::Class {
                name,
                super_type,
                members,
                doc_str,
            }
        },
    )(input)
}

fn module(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    let (input, doc_str) = comment_block(input)?;
    let (input, name) = preceded(ws(tag("module")), identifier)(input)?;
    let (input, members) = block(many0(multi_ws(module_declaration)))(input)?;
    Ok((
        input,
        Node::Module {
            name,
            members,
            doc_str,
        },
    ))
}

fn import(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    map(preceded(ws(tag("import")), sub_identifier),|name|{
        Node::Import { name }
    })(input)
}

fn module_declaration(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    alt((import, enum_type, class_type, module))(input)
}

pub fn parse_schema(input: &str) -> IResult<Span, Vec<Node>, VerboseError<Span>> {
    let input = input.into();
    terminated(many0(multi_ws(module_declaration)), eof)(input)
}

// Element example
// server = Process{
//      os = "FreeBSD-14"
// }
fn element_assign(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    map(
        tuple((
            comment_block,
            preceded(space0, identifier),
            assign,
            identifier,
            block(terminated(
                separated_list0(comma, object_assignment),
                opt(comma),
            )),
        )),
        |(doc_str, name, _, datatype, members)| Node::Element {
            name,
            datatype,
            members,
            doc_str,
        },
    )(input)
}

// Flow example
// client -> server = Type {}
fn flow_assign(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    map(
        tuple((
            comment_block,
            preceded(space0, identifier),
            ws(tag("->")),
            identifier,
            assign,
            identifier,
            block(terminated(
                separated_list0(comma, object_assignment),
                opt(comma),
            )),
        )),
        |(doc_str, from, _, to, _, datatype, members)| Node::Flow {
            from,
            to,
            datatype,
            members,
            doc_str,
        },
    )(input)
}

fn sequence(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    map(
        tuple((
            comment_block,
            preceded(space0, tag("seq")),
            opt(ws(identifier)),
            block(many0(flow_assign))
        )),
        |(doc_str, _,name, flows)| Node::Sequence { name, flows, doc_str }
    )(input)


}

fn model_declaration(input: Span) -> IResult<Span, Node, VerboseError<Span>> {
    alt((module_declaration, element_assign, flow_assign))(input)
}

pub fn parse_model<'input>(input: &'input str) -> Result<Vec<Node<'input>>, VerboseError<Span>> {
    let (_rest, ast) = terminated(many0(multi_ws(model_declaration)), eof)(input).finish()?;
    Ok(ast)
}

#[cfg(test)]
mod test {
    use super::*;
    use core::fmt::Debug;

    fn check_rest<T: Debug>(result: IResult<Span, T, VerboseError<Span>>) -> T {
        let (rest, value) = result.unwrap();
        assert_eq!(rest, "");
        value
    }

    #[test]
    fn test_comment() {
        let result = check_rest(comment_line(" # "));
        assert_eq!(result, "");
    }
    #[test]
    fn test_comment2() {
        let result = check_rest(comment_line(" # lol  asdasd"));
        assert_eq!(result, "lol  asdasd");
    }

    #[test]
    fn test_comment_single_block() {
        let result = check_rest(comment_block("# lol222\n"));
        assert_eq!(result, vec!["lol222"]);
    }

    #[test]
    fn test_comment_block() {
        let result = check_rest(comment_block(
            "# lol
                # lol2\n"
                ,
        ));
        assert_eq!(result, vec!["lol", "lol2"]);
    }
    #[test]
    fn test_comment_block_lead_newline() {
        let result = check_rest(comment_block(
            "

                # lol
                # lol2\n"
                ,
        ));
        assert_eq!(result, vec!["lol", "lol2"]);
    }

    #[test]
    fn test_class() {
        let result = check_rest(parse_schema(
            "
                type File { bool:Bool, test:String, }",
        ));
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
        let result = check_rest(parse_schema(" type File { bool = true, test:String, }"));
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
        let result = check_rest(parse_schema(" type File:Super { bool:Bool, }"));
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
        let result = check_rest(parse_schema(" enum Level { Test, Lol }"));
        if let Node::Enum {
            name,
            members,
            doc_str,
        } = &result[0]
        {
            assert_eq!(*name, "Level");
            assert_eq!(members[0], "Test");
            assert_eq!(members[1], "Lol");
        } else {
            assert!(false);
        }
    }
    #[test]
    fn test_module() {
        let result = check_rest(parse_schema(" module mqtt { enum Publish{Bla, }}"));
        if let Node::Module { name, members, .. } = &result[0] {
            assert_eq!(*name, "mqtt");
            assert_eq!(
                members[0],
                Node::Enum {
                    name: "Publish",
                    members: vec!["Bla"],
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

enum Severity {
    High,
    Low,
}
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
        let result = check_rest(parse_schema(test_str));
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
        assert_eq!(check_rest(bool("true")), Constant::Bool(true));
        assert_eq!(check_rest(bool("false")), Constant::Bool(false));
    }
    #[test]
    fn test_string() {
        assert_eq!(check_rest(string("\"\"")), Constant::Str("".into()));
        assert_eq!(
            check_rest(string("\"test\"")),
            Constant::Str("test")
        );
        //assert_eq!(check_rest(string("\"te_st\"")), Term::Str("test"));
        //assert_eq!(check_rest(string("\"te  st\"")), Term::Str("te  st"));
    }
//    #[test]
//    fn test_atom() {
//        assert_eq!(
//            check_rest(atom("test.test")),
//            Constant::Atom("test.test")
//        );
//    }
    #[test]
    fn test_int() {
        assert_eq!(check_rest(int("15")), Constant::Int(15));
        assert_eq!(check_rest(int("0x15")), Constant::Int(0x15));
        assert_eq!(check_rest(int("0xFE")), Constant::Int(0xfe));
        assert_eq!(check_rest(int("-15")), Constant::Int(-15));
        assert_eq!(check_rest(int("0")), Constant::Int(0));
    }
    #[test]
    fn test_member() {
        assert_eq!(
            check_rest(member::<0>(".a")),
            Operation::Member {
                name: ".a".into(),
                pos: 0
            }
        );
        assert_eq!(
            check_rest(member::<0>(".a.b")),
            Operation::Member {
                name: ".a.b".into(),
                pos: 0
            }
        );
        assert_eq!(
            check_rest(member::<0>(".a.b.xyz")),
            Operation::Member {
                name: ".a.b.xyz".into(),
                pos: 0
            }
        );
    }
    #[test]
    fn test_comp_op() {
        assert_eq!(check_rest(comp_op("==")), Operation::Equal);
        assert_eq!(check_rest(comp_op("!=")), Operation::NotEqual);
        assert_eq!(check_rest(comp_op("<")), Operation::Lesser);
        assert_eq!(check_rest(comp_op(">")), Operation::Greater);
        assert_eq!(check_rest(comp_op("<=")), Operation::LesserEq);
        assert_eq!(check_rest(comp_op(">=")), Operation::GreaterEq);
    }
    #[test]
    fn test_comp_expr() {
        assert_eq!(
            check_rest(comp_expr(".a==.b")),
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
            check_rest(comp_expr(".a!=5")),
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
            check_rest(comp_expr(".abc<\"lol\"")),
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
            check_rest(comp_expr("4>.abc")),
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
            check_rest(comp_expr("42 > .abc")),
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
            check_rest(comp_expr("true")),
            vec![Operation::Bool(true)]
        );
        assert_eq!(check_rest(comp_expr("5")), vec![Operation::Int(5)]);
    }
    #[test]
    fn test_not_expr() {
        assert_eq!(
            check_rest(comp_expr("42 > not .abc")),
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
            check_rest(comp_expr("not true")),
            vec![Operation::Bool(true), Operation::Not]
        );
        assert_eq!(
            check_rest(comp_expr("not 5")),
            vec![Operation::Int(5), Operation::Not]
        );
    }
    #[test]
    fn test_logic_expr() {
        assert_eq!(
            check_rest(logic_and(".a==.b and .abc")),
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
            check_rest(logic_and(".a == .b    and    .abc")),
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
            check_rest(logic_and(".a == .b and .abc and .test")),
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
            check_rest(logic_or(".a == .b or .abc and .test")),
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
            check_rest(logic_or("(.a == .b or .abc) and .test")),
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
            check_rest(parse_query(
                "  ( .a ==  .b   or   .abc )   and   not .test  "
            )),
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
