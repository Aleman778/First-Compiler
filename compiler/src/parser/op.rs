
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
impl Parser for BinOp {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "operator",
            alt((
                map(tag("+"),  |s| BinOp::Add{span: Span::new(s)}),
                map(tag("-"),  |s| BinOp::Sub{span: Span::new(s)}),
                map(tag("**"), |s| BinOp::Pow{span: Span::new(s)}),
                map(tag("*"),  |s| BinOp::Mul{span: Span::new(s)}),
                map(tag("/"),  |s| BinOp::Div{span: Span::new(s)}),
                map(tag("%"),  |s| BinOp::Mod{span: Span::new(s)}),
                map(tag("&&"), |s| BinOp::And{span: Span::new(s)}),
                map(tag("||"), |s| BinOp::Or{span: Span::new(s)}),
                map(tag("=="), |s| BinOp::Eq{span: Span::new(s)}),
                map(tag("!="), |s| BinOp::Ne{span: Span::new(s)}),
                map(tag("<"),  |s| BinOp::Lt{span: Span::new(s)}),
                map(tag("<="), |s| BinOp::Le{span: Span::new(s)}),
                map(tag(">"),  |s| BinOp::Gt{span: Span::new(s)}),
                map(tag(">="), |s| BinOp::Ge{span: Span::new(s)}),
            ))
        )(input)
    }
}


impl Parser for UnOp {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "operator",
            alt((
                map(tag("-"),  |s| UnOp::Neg{span: Span::new(s)}),
                map(tag("!"),  |s| UnOp::Not{span: Span::new(s)}),
                map(tag("&"),  |s| UnOp::Ref{span: Span::new(s)}),
                map(tag("*"),  |s| UnOp::Deref{span: Span::new(s)}),
            ))
        )(input)
    }    
}
