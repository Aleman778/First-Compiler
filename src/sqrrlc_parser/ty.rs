
/***************************************************************************
 * Type Parser implementation defines parsers for all the supported types.
 ***************************************************************************/


use nom::{
    character::complete::{multispace0, multispace1},
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{preceded, tuple},
    branch::alt,
    error::context,
};
use crate::sqrrlc_ast::{
    span::{Span, LineColumn},
    ty::*,
};
use crate::sqrrlc_parser::{
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parse a type can be i32, bool or a reference.
 */
impl Parser for Ty {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "type",
            map(TyKind::parse, |(kind, span)| Ty{kind: kind, span: span})
        )(input)    
    }
}

impl TyKind {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, (Self, Span)> {
        alt((
            map(preceded(multispace0, tag("i32")), |s| (TyKind::Int(IntTy::I32), Span::new(s))),
            map(preceded(multispace0, tag("i64")), |s| (TyKind::Int(IntTy::I64), Span::new(s))),
            map(preceded(multispace0, tag("bool")), |s| (TyKind::Bool, Span::new(s))),
            map(preceded(multispace0, TypeRef::parse), |r| {
                let span = r.span.clone();
                (TyKind::Ref(r), span)
            }),
        ))(input)
    }
}


/**
 * Parse a type reference or mutable reference.
 */
impl Parser for TypeRef {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "type reference",
            map(tuple((
                preceded(multispace0, tag("&")),
                opt(preceded(multispace0, tag("mut"))),
                preceded(multispace1, Ty::parse),
            )),
                |(and, mutability, elem)| {
                    let elem_span = elem.span.clone();
                    TypeRef {
                        mutability: mutability.is_some(),
                        elem: Box::new(elem),
                        span: Span::from_bounds(
                            LineColumn::new(and.line, and.get_column()), elem_span.end,
                        )
                    }
                }
            )
        )(input)
    }
}