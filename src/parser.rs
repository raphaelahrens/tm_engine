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

mod query;
pub mod model;
pub mod types;

use std::ops::RangeInclusive;

use winnow::{
    ascii::{
        digit1, hex_digit1, line_ending, multispace0, multispace1, space0, space1, till_line_ending,
    },
    combinator::{
        alt, cut_err, delimited, opt, preceded, repeat, separated, terminated, Repeat
    },
    error::{ContextError, ParserError, StrContext},
    prelude::*,
    token::{any, none_of, one_of, take_while},
    Located,
};
use line_span::LineSpanExt;

pub use self::query::{
    parse_tql,
    parse_query,
    compile_query,
    QNode,
};

pub use self::model:: parse_model;

pub fn get_line_from_offset(source_code: &str, offset:usize) -> &str{
        &source_code[source_code.find_line_range(offset)]
}

pub type ParseError<'a> = winnow::error::ParseError<Stream<'a>, ContextError>;

const ALPHA: (RangeInclusive<char>, RangeInclusive<char>) = ('a'..='z', 'A'..='Z');
const DIGIT: RangeInclusive<char> = '0'..='9';
pub type Stream<'i> = Located<&'i str>;

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.

fn ws<'i, F, O, E>(inner: F) -> impl Parser<Stream<'i>, O, E>
where
    E: ParserError<Stream<'i>>
        + for<'a> winnow::error::AddContext<Stream<'a>, winnow::error::StrContext>,
    F: Parser<Stream<'i>, O, E>,
{
    delimited(
        space0.context(StrContext::Label("leading space")),
        inner.context(StrContext::Label("inner")),
        space0.context(StrContext::Label("trailing space")),
    )
}
fn ws1<'i, F, O, E>(inner: F) -> impl Parser<Stream<'i>, O, E>
where
    E: ParserError<Stream<'i>>
        + for<'a> winnow::error::AddContext<Stream<'a>, winnow::error::StrContext>,
    F: Parser<Stream<'i>, O, E>,
{
    delimited(
        space1.context(StrContext::Label("leading space")),
        inner.context(StrContext::Label("inner")),
        space1.context(StrContext::Label("trailing space")),
    )
}

fn multi_ws<'i, F, O, E>(inner: F) -> impl Parser<Stream<'i>, O, E>
where
    E: ParserError<Stream<'i>>
        + for<'a> winnow::error::AddContext<Stream<'a>, winnow::error::StrContext>,
    F: Parser<Stream<'i>, O, E>,
{
    delimited(
        multispace0.context(StrContext::Label("leading space")),
        inner.context(StrContext::Label("inner")),
        multispace0.context(StrContext::Label("trailing space")),
    )
}
fn multi_ws1<'i, F, O, E>(inner: F) -> impl Parser<Stream<'i>, O, E>
where
    E: ParserError<Stream<'i>>
        + for<'a> winnow::error::AddContext<Stream<'a>, winnow::error::StrContext>,
    F: Parser<Stream<'i>, O, E>,
{
    delimited(
        multispace1.context(StrContext::Label("leading space")),
        inner.context(StrContext::Label("inner")),
        multispace1.context(StrContext::Label("trailing space")),
    )
}

fn block<'i, F, O, E>(inner: F) -> impl Parser<Stream<'i>, O, E>
where
    E: ParserError<Stream<'i>>
        + for<'a> winnow::error::AddContext<Stream<'a>, winnow::error::StrContext>,
    F: Parser<Stream<'i>, O, E>,
{
    delimited(
        multi_ws('{').context(StrContext::Label("{")),
        inner,
        multi_ws(
            cut_err('}'.context(StrContext::Label("}")))
            ),
    )
}

fn comment_line<'i>(input: &mut Stream<'i>) -> PResult<&'i str> {
    preceded(ws('#'), till_line_ending).parse_next(input)
}

fn comment_block<'i>(input: &mut Stream<'i>) -> PResult<Vec<&'i str>> {
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


#[derive(Debug, PartialEq)]
pub enum Constant<'input> {
    Bool(bool),
    Str(&'input str),
    Int(i64),
    List(Vec<Constant<'input>>),
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

fn sub_identifier<'i, const N: usize>(input: &mut Stream<'i>) -> PResult<Vec<&'i str>> {
    //let prefix: Repeat<_, _, _, (), _> = repeat(N.., preceded(identifier, '.'));
    //preceded(prefix, identifier).recognize().parse_next(input)
    separated(N.., identifier, '.').parse_next(input)
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


fn list_constant<'i>(input: &mut Stream<'i>) -> PResult<Constant<'i>> {
    delimited('[', separated(0.., value_constant, ','), ']').map(|list| Constant::List(list)).parse_next(input)
}

fn value_constant<'i>(input: &mut Stream<'i>) -> PResult<Constant<'i>> {
    alt((
            bool,
            int,
            string,
            list_constant
        )).parse_next(input)
}

#[derive(Debug, PartialEq)]
struct Enum<'input>{
    datatype: Vec<&'input str>,
    value: &'input str
}

fn enum_value<'i>(input: &mut Stream<'i>) -> PResult<Enum<'i>> {
    sub_identifier::<2>
        .with_span()
        .map(|(mut items, _r)| {
            let Some(value) = items.pop() else {todo!("can't happen since we had succefully parsed 2 sub_identifiers")};
            Enum{
                datatype: items,
                value
            }
        })
        .parse_next(input)
}

fn import<'i>(input: &mut Stream<'i>) -> PResult<Vec<&'i str>> {
    preceded(ws("import"), sub_identifier::<1>)
        .parse_next(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_comment() {
        let result = comment_line(&mut Stream::new(" # ")).unwrap();
        assert_eq!(result, "");
    }
    #[test]
    fn test_comment2() {
        let result = comment_line(&mut Stream::new(" # lol  asdasd")).unwrap();
        assert_eq!(result, "lol  asdasd");
    }

    #[test]
    fn test_comment_single_block() {
        let result = comment_block(&mut Stream::new("# lol222\n")).unwrap();
        assert_eq!(result, vec!["lol222"]);
    }

    #[test]
    fn test_comment_block() {
        let result = comment_block(&mut Stream::new(
            "# lol
                # lol2\n",
        ))
        .unwrap();
        assert_eq!(result, vec!["lol", "lol2"]);
    }
    #[test]
    fn test_comment_block_lead_newline() {
        let result = comment_block(&mut Stream::new(
            "

                # lol
                # lol2\n",
        ))
        .unwrap();
        assert_eq!(result, vec!["lol", "lol2"]);
    }

    #[test]
    fn test_bool() {
        assert_eq!(
            bool(&mut Stream::new("true")).unwrap(),
            Constant::Bool(true)
        );
        assert_eq!(
            bool(&mut Stream::new("false")).unwrap(),
            Constant::Bool(false)
        );
    }
    #[test]
    fn test_string() {
        assert_eq!(
            string(&mut Stream::new("\"\"")).unwrap(),
            Constant::Str("".into())
        );
        assert_eq!(
            string(&mut Stream::new("\"test\"")).unwrap(),
            Constant::Str("test")
        );
    }
    #[test]
    fn test_int() {
        assert_eq!(int(&mut Stream::new("15")).unwrap(), Constant::Int(15));
        assert_eq!(int(&mut Stream::new("0x15")).unwrap(), Constant::Int(0x15));
        assert_eq!(int(&mut Stream::new("0xFE")).unwrap(), Constant::Int(0xfe));
        assert_eq!(int(&mut Stream::new("-15")).unwrap(), Constant::Int(-15));
        assert_eq!(int(&mut Stream::new("0")).unwrap(), Constant::Int(0));
    }
}
