
/***************************************************************************
 * Expression parser implementation defines parsers for any
 * type of expression such as let statements, binary operations etc.
 ***************************************************************************/


use nom::{
    character::complete::{alpha1, digit1, multispace0},
    character::{is_alphanumeric},
    bytes::complete::{tag, take_while1},
    combinator::{map, peek},
    sequence::{preceded, pair, tuple},
    branch::alt,
    error::context,
};


use crate::ast::{
    span::Span,
    atom::Atom,
    expr::*,
    op::*,
};


use crate::parser::{
    error::{ParseError, ErrorKind},
    ParseSpan,
    IResult,
    Parser,
};


/* TEMPLATE USED FOR COPYING

impl Parser for  {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "",
        )(input)
    }
}

*/



impl Parser for Expr {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "expression",
            alt((
                map(ExprBinOp::parse, |res| Expr::BinOp(res)),
                map(Atom::parse, |res| Expr::Atom(res)),
            ))
        )(input)
    }
}


/**
 * Parse binary operations e.g. "10 + 5", "false && true" etc.
 */
impl Parser for ExprBinOp {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "binary operation",
            map(tuple((Atom::parse, BinOp::parse, Expr::parse)),
                |(left, op, right)| ExprBinOp{
                    left: Box::new(left),
                    op: op,
                    right: Box::new(right),
                    span: Span::new(input)})
        )(input)
    }
}


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



/**
 * Parse unary operators
 */
impl Parser for UnOp {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "operator",
            alt((
                map(tag("*"),  |s| UnOp::Deref{span: Span::new(s)}),
                map(tag("!"),  |s| UnOp::Not{span: Span::new(s)}),
                map(tag("-"),  |s| UnOp::Neg{span: Span::new(s)}),
            ))
        )(input)
    }
}
