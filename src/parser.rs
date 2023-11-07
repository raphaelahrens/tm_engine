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
        alpha1, alphanumeric0, alphanumeric1, char, hex_digit1, i64, multispace1, space0,
    },
    combinator::{map, map_res, opt, recognize, value, eof},
    multi::{many0_count, many1},
    sequence::{delimited, pair, preceded, tuple, terminated},
    IResult,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Term<'a> {
    Bool(bool),
    Str(&'a str),
    Atom(&'a str),
    Int(i64),
    Member(Vec<&'a str>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompOperator {
    Greater,
    GreaterEq,
    Equal,
    LesserEq,
    Lesser,
    NotEqual,
}

#[derive(Debug, PartialEq)]
pub enum Comp<'a> {
    Expr {
        a: Box<Comp<'a>>,
        op: CompOperator,
        b: Box<Comp<'a>>,
    },
    //TODO not is in the wrong place this allows ! .test == abc. to be equal !(.test == .abc)
    Not(Box<Comp<'a>>),
    Term(Term<'a>),
    Parent(Box<Logic<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Logic<'a> {
    Or {
        a: Box<Logic<'a>>,
        b: Box<Logic<'a>>,
    },
    And {
        a: Comp<'a>,
        b: Box<Logic<'a>>,
    },
    Comp(Comp<'a>),
}

fn bool<'a>(input: &'a str) -> IResult<&'a str, Term<'a>> {
    alt((
        value(Term::Bool(true), tag("true")),
        value(Term::Bool(false), tag("false")),
    ))(input)
}

fn mark(input: &str) -> IResult<&str, &str> {
    tag("'")(input)
}
fn quote(input: &str) -> IResult<&str, char> {
    char('"')(input)
}

fn string<'a>(input: &'a str) -> IResult<&'a str, Term<'a>> {
    map(delimited(quote, alphanumeric0, quote), |s| Term::Str(s))(input)
}

fn atom<'a>(input: &'a str) -> IResult<&'a str, Term<'a>> {
    map(preceded(mark, alphanumeric1), |s| Term::Atom(s))(input)
}

fn int<'a>(input: &'a str) -> IResult<&'a str, Term<'a>> {
    let hex_num = map_res(preceded(tag("0x"), hex_digit1), |s| {
        i64::from_str_radix(s, 16)
    });
    let dec_num = map(tuple((opt(tag("-")), i64)), |(sign, num)| match sign {
        None => num,
        Some(_) => -num,
    });
    map(alt((hex_num, dec_num)), |s| Term::Int(s))(input)
}

fn constant<'a>(input: &'a str) -> IResult<&'a str, Term<'a>> {
    alt((bool, int, atom, string))(input)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_"))))))(input)
}

fn member<'a>(input: &'a str) -> IResult<&'a str, Term<'a>> {
    let attribute = preceded(char('.'), identifier);
    map(many1(attribute), |v| Term::Member(v))(input)
}

fn equal(input: &str) -> IResult<&str, CompOperator> {
    value(CompOperator::Equal, tag("=="))(input)
}
fn not_equal(input: &str) -> IResult<&str, CompOperator> {
    value(CompOperator::NotEqual, tag("!="))(input)
}
fn greater(input: &str) -> IResult<&str, CompOperator> {
    value(CompOperator::Greater, tag(">"))(input)
}
fn greater_eq(input: &str) -> IResult<&str, CompOperator> {
    value(CompOperator::GreaterEq, tag(">="))(input)
}
fn lesser(input: &str) -> IResult<&str, CompOperator> {
    value(CompOperator::Lesser, tag("<"))(input)
}
fn lesser_eq(input: &str) -> IResult<&str, CompOperator> {
    value(CompOperator::LesserEq, tag("<="))(input)
}

fn comp_op(input: &str) -> IResult<&str, CompOperator> {
    alt((equal, not_equal, greater_eq, lesser_eq, greater, lesser))(input)
}

fn term<'a>(input: &'a str) -> IResult<&'a str, Term<'a>> {
    alt((member, constant))(input)
}

fn not_expr<'a>(input: &'a str) -> IResult<&'a str, Comp<'a>> {
    alt((
        map(preceded(tuple((tag("!"), space0)), not_expr), |e| {
            Comp::Not(Box::new(e))
        }),
        map(term, |t| Comp::Term(t)),
    ))(input)
}

fn parents_expr<'a>(input: &'a str) -> IResult<&'a str, Comp<'a>> {
    map(
        delimited(
            tuple((tag("("), space0)),
            logic_or,
            tuple((space0, tag(")"))),
        ),
        |e| Comp::Parent(Box::new(e)),
    )(input)
}

fn comp_expr<'a>(input: &'a str) -> IResult<&'a str, Comp<'a>> {
    alt((
        map(
            tuple((not_expr, space0, comp_op, space0, not_expr)),
            |(a, _, op, _, b)| Comp::Expr {
                a: Box::new(a),
                op,
                b: Box::new(b),
            },
        ),
        not_expr,
        parents_expr,
    ))(input)
}

fn logic_and<'a>(input: &'a str) -> IResult<&'a str, Logic<'a>> {
    alt((
        map(
            tuple((comp_expr, multispace1, tag("and"), multispace1, logic_and)),
            |(a, _, _, _, b)| Logic::And { a, b: Box::new(b) },
        ),
        map(comp_expr, |t| Logic::Comp(t)),
    ))(input)
}

fn logic_or<'a>(input: &'a str) -> IResult<&'a str, Logic<'a>> {
    alt((
        map(
            tuple((logic_and, multispace1, tag("or"), multispace1, logic_or)),
            |(a, _, _, _, b)| Logic::Or {
                a: Box::new(a),
                b: Box::new(b),
            },
        ),
        logic_and,
    ))(input)
}

pub fn parse_query<'a>(input: &'a str) -> IResult<&'a str, Logic<'a>> {
    terminated(logic_or, eof)(input)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_bool() {
        assert_eq!(bool("true"), Ok(("", Term::Bool(true))));
        assert_eq!(bool("false"), Ok(("", Term::Bool(false))));
    }
    #[test]
    fn test_string() {
        assert_eq!(string("\"\""), Ok(("", Term::Str(""))));
        assert_eq!(string("\"test\""), Ok(("", Term::Str("test"))));
        //assert_eq!(string("\"te_st\""), Ok(("", Term::Str("test"))));
        //assert_eq!(string("\"te  st\""), Ok(("", Term::Str("te  st"))));
    }
    #[test]
    fn test_atom() {
        assert_eq!(atom("'test"), Ok(("", Term::Atom("test"))));
        assert_eq!(atom("'1test"), Ok(("", Term::Atom("1test"))));
    }
    #[test]
    fn test_int() {
        assert_eq!(int("15"), Ok(("", Term::Int(15))));
        assert_eq!(int("0x15"), Ok(("", Term::Int(0x15))));
        assert_eq!(int("0xFE"), Ok(("", Term::Int(0xfe))));
        assert_eq!(int("-15"), Ok(("", Term::Int(-15))));
        assert_eq!(int("0"), Ok(("", Term::Int(0))));
    }
    #[test]
    fn test_member() {
        assert_eq!(member(".a"), Ok(("", Term::Member(vec!["a"]))));
        assert_eq!(member(".a.b"), Ok(("", Term::Member(vec!["a", "b"]))));
        assert_eq!(
            member(".a.b.xyz"),
            Ok(("", Term::Member(vec!["a", "b", "xyz"])))
        );
    }
    #[test]
    fn test_comp_op() {
        assert_eq!(comp_op("=="), Ok(("", CompOperator::Equal)));
        assert_eq!(comp_op("!="), Ok(("", CompOperator::NotEqual)));
        assert_eq!(comp_op("<"), Ok(("", CompOperator::Lesser)));
        assert_eq!(comp_op(">"), Ok(("", CompOperator::Greater)));
        assert_eq!(comp_op("<="), Ok(("", CompOperator::LesserEq)));
        assert_eq!(comp_op(">="), Ok(("", CompOperator::GreaterEq)));
    }
    #[test]
    fn test_comp_expr() {
        assert_eq!(
            comp_expr(".a==.b"),
            Ok((
                "",
                Comp::Expr {
                    a: Box::new(Comp::Term(Term::Member(vec!["a"]))),
                    op: CompOperator::Equal,
                    b: Box::new(Comp::Term(Term::Member(vec!["b"])))
                }
            ))
        );
        assert_eq!(
            comp_expr(".a!=5"),
            Ok((
                "",
                Comp::Expr {
                    a: Box::new(Comp::Term(Term::Member(vec!["a"]))),
                    op: CompOperator::NotEqual,
                    b: Box::new(Comp::Term(Term::Int(5)))
                }
            ))
        );
        assert_eq!(
            comp_expr(".abc<\"lol\""),
            Ok((
                "",
                Comp::Expr {
                    a: Box::new(Comp::Term(Term::Member(vec!["abc"]))),
                    op: CompOperator::Lesser,
                    b: Box::new(Comp::Term(Term::Str("lol")))
                }
            ))
        );
        assert_eq!(
            comp_expr("'abc>.abc"),
            Ok((
                "",
                Comp::Expr {
                    a: Box::new(Comp::Term(Term::Atom("abc"))),
                    op: CompOperator::Greater,
                    b: Box::new(Comp::Term(Term::Member(vec!["abc"]))),
                }
            ))
        );
        assert_eq!(
            comp_expr("'abc > .abc"),
            Ok((
                "",
                Comp::Expr {
                    a: Box::new(Comp::Term(Term::Atom("abc"))),
                    op: CompOperator::Greater,
                    b: Box::new(Comp::Term(Term::Member(vec!["abc"]))),
                }
            ))
        );
        assert_eq!(comp_expr("true"), Ok(("", Comp::Term(Term::Bool(true),))));
        assert_eq!(comp_expr("5"), Ok(("", Comp::Term(Term::Int(5),))));
        //assert_eq!(comp_expr("'test<='lol"), Ok(("", CompOperator::LesserEq)));
        //assert_eq!(comp_expr(".abc.test>=0x55"), Ok(("", CompOperator::GreaterEq)));
    }
    #[test]
    fn test_not_expr() {
        assert_eq!(
            comp_expr("'abc > .abc"),
            Ok((
                "",
                Comp::Expr {
                    a: Box::new(Comp::Term(Term::Atom("abc"))),
                    op: CompOperator::Greater,
                    b: Box::new(Comp::Term(Term::Member(vec!["abc"]))),
                }
            ))
        );
        assert_eq!(comp_expr("true"), Ok(("", Comp::Term(Term::Bool(true),))));
        assert_eq!(comp_expr("5"), Ok(("", Comp::Term(Term::Int(5),))));
    }
    #[test]
    fn test_logic_expr() {
        assert_eq!(
            logic_and(".a==.b and .abc"),
            Ok((
                "",
                Logic::And {
                    a: Comp::Expr {
                        a: Box::new(Comp::Term(Term::Member(vec!["a"]))),
                        op: CompOperator::Equal,
                        b: Box::new(Comp::Term(Term::Member(vec!["b"]))),
                    },
                    b: Box::new(Logic::Comp(Comp::Term(Term::Member(vec!["abc"])))),
                }
            ))
        );
        assert_eq!(
            logic_and(".a == .b    and    .abc"),
            Ok((
                "",
                Logic::And {
                    a: Comp::Expr {
                        a: Box::new(Comp::Term(Term::Member(vec!["a"]))),
                        op: CompOperator::Equal,
                        b: Box::new(Comp::Term(Term::Member(vec!["b"]))),
                    },
                    b: Box::new(Logic::Comp(Comp::Term(Term::Member(vec!["abc"])),)),
                }
            ))
        );
        assert_eq!(
            logic_and(".a == .b and .abc and .test"),
            Ok((
                "",
                Logic::And {
                    a: Comp::Expr {
                        a: Box::new(Comp::Term(Term::Member(vec!["a"]))),
                        op: CompOperator::Equal,
                        b: Box::new(Comp::Term(Term::Member(vec!["b"]))),
                    },
                    b: Box::new(Logic::And {
                        a: Comp::Term(Term::Member(vec!["abc"])),
                        b: Box::new(Logic::Comp(Comp::Term(Term::Member(vec!["test"]))))
                    })
                }
            ))
        );
        assert_eq!(
            logic_or(".a == .b or .abc and .test"),
            Ok((
                "",
                Logic::Or {
                    a: Box::new(Logic::Comp(Comp::Expr {
                        a: Box::new(Comp::Term(Term::Member(vec!["a"]))),
                        op: CompOperator::Equal,
                        b: Box::new(Comp::Term(Term::Member(vec!["b"]))),
                    })),
                    b: Box::new(Logic::And {
                        a: Comp::Term(Term::Member(vec!["abc"])),
                        b: Box::new(Logic::Comp(Comp::Term(Term::Member(vec!["test"]))))
                    })
                }
            ))
        );
        assert_eq!(
            logic_or("(.a == .b or .abc) and .test"),
            Ok((
                "",
                Logic::And {
                    a: Comp::Parent(Box::new(Logic::Or {
                        a: Box::new(Logic::Comp(Comp::Expr {
                            a: Box::new(Comp::Term(Term::Member(vec!["a"]))),
                            op: CompOperator::Equal,
                            b: Box::new(Comp::Term(Term::Member(vec!["b"]))),
                        })),
                        b: Box::new(Logic::Comp(Comp::Term(Term::Member(vec!["abc"]))),),
                    })),
                    b: Box::new(Logic::Comp(Comp::Term(Term::Member(vec!["test"])))),
                }
            ))
        );
    }
    #[test]
    fn example_query() {
        assert_eq!(
        parse_query("(.a == .b or .abc) and !.test"),
            Ok((
                "",
                Logic::And {
                    a: Comp::Parent(Box::new(Logic::Or {
                        a: Box::new(Logic::Comp(Comp::Expr {
                            a: Box::new(Comp::Term(Term::Member(vec!["a"]))),
                            op: CompOperator::Equal,
                            b: Box::new(Comp::Term(Term::Member(vec!["b"]))),
                        })),
                        b: Box::new(Logic::Comp(Comp::Term(Term::Member(vec!["abc"]))),),
                    })),
                    b: Box::new(Logic::Comp(Comp::Not(Box::new(Comp::Term(Term::Member(vec!["test"])))))),
                }
            ))
        );
    }
}
