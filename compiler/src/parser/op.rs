
/***************************************************************************
 * Parse operators either binary or unary operators.
 ***************************************************************************/


use crate::ast::{op::*, span::Span};
use crate::parser::{ParseSpan, Parser, IResult};
use nom::{
    bytes::complete::tag,
    combinator::map,
    error::context,
    branch::alt,
};


/**
 * Parse binary operators
 */
impl Parser for Op {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "operator",
            alt((
                map(tag("+"),  |s| Op::Add{span: Span::new(s)}),
                map(tag("-"),  |s| Op::Sub{span: Span::new(s)}),
                map(tag("**"), |s| Op::Pow{span: Span::new(s)}),
                map(tag("*"),  |s| Op::Mul{span: Span::new(s)}),
                map(tag("/"),  |s| Op::Div{span: Span::new(s)}),
                map(tag("%"),  |s| Op::Mod{span: Span::new(s)}),
                map(tag("&&"), |s| Op::And{span: Span::new(s)}),
                map(tag("||"), |s| Op::Or{span: Span::new(s)}),
                map(tag("=="), |s| Op::Eq{span: Span::new(s)}),
                map(tag("!="), |s| Op::Ne{span: Span::new(s)}),
                map(tag("<"),  |s| Op::Lt{span: Span::new(s)}),
                map(tag("<="), |s| Op::Le{span: Span::new(s)}),
                map(tag(">"),  |s| Op::Gt{span: Span::new(s)}),
                map(tag(">="), |s| Op::Ge{span: Span::new(s)}),
                map(tag("!"),  |s| Op::Not{span: Span::new(s)}),
            ))
        )(input)
    }
}
