use winnow::{
    combinator::{
        alt, delimited, separated,
    },
    prelude::*,
};

use super::{
    identifier,
    Stream,
    ws,
};

/**
 * A [UnionType] is a set of [Datatype]s.
 *
 *
 */
#[derive(Debug, PartialEq)]
pub struct UnionType<'input>(pub Vec<Datatype<'input>>);

impl <'input> UnionType<'input> {
    pub fn iter(&self) -> std::slice::Iter<Datatype> {
        self.0.iter()
    }
}

#[derive(Debug, PartialEq)]
pub enum Datatype<'input> {
    List(UnionType<'input>),
    //Datatype can be a Vec since we can have a union of multiple types
    SingleType(&'input str)
}


fn named_type<'i>(input: &mut Stream<'i>) -> PResult<Datatype<'i>> {
    identifier.map(|name| {Datatype::SingleType(name)})
        .parse_next(input)
}

pub fn type_declartion<'i>(input: &mut Stream<'i>) -> PResult<UnionType<'i>> {
    separated(1.., list_type, ws('|')).map(
        |types|UnionType(types)
        )
        .parse_next(input)
}
fn list_type<'i>(input: &mut Stream<'i>) -> PResult<Datatype<'i>> {
    alt((
        named_type,
        delimited('[', type_declartion, ']') .map(|inner_type|{
            Datatype::List(inner_type)
        })
    )).parse_next(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_uniontype() {
        let result = type_declartion(&mut Stream::new("Int")).unwrap();
        assert_eq!(result, UnionType(vec![Datatype::SingleType("Int")]));
        let result = type_declartion(&mut Stream::new("Int|String")).unwrap();
        assert_eq!(result, UnionType(vec![Datatype::SingleType("Int"), Datatype::SingleType("String")]));
        let result = type_declartion(&mut Stream::new("Int | String")).unwrap();
        assert_eq!(result, UnionType(vec![Datatype::SingleType("Int"), Datatype::SingleType("String")]));
        let result = type_declartion(&mut Stream::new("[Int]")).unwrap();
        assert_eq!(result, UnionType(vec![Datatype::List(UnionType(vec![Datatype::SingleType("Int")]))]));
        let result = type_declartion(&mut Stream::new("[Int|String]")).unwrap();
        assert_eq!(result, UnionType(vec![Datatype::List(UnionType(vec![Datatype::SingleType("Int"), Datatype::SingleType("String")]))]));
        let result = type_declartion(&mut Stream::new("[[Int|String]]")).unwrap();
        assert_eq!(result, UnionType(
                vec![
                Datatype::List(
                    UnionType(
                        vec![
                        Datatype::List(
                            UnionType(
                                vec![
                                Datatype::SingleType("Int"),
                                Datatype::SingleType("String")
                                ]
                                )
                            )
                        ]
                        )
                    )
                ]
                )
            );
        let result = type_declartion(&mut Stream::new("[[Int|String] | Int]")).unwrap();
        assert_eq!(result, UnionType(
                vec![
                Datatype::List(
                    UnionType(
                        vec![
                        Datatype::List(
                            UnionType(
                                vec![
                                Datatype::SingleType("Int"),
                                Datatype::SingleType("String")
                                ]
                                )
                            ),
                            Datatype::SingleType("Int"),
                        ]
                        )
                    )
                ]
                )
            );
    }
}
