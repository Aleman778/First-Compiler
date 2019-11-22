
/***************************************************************************
 * Statement parser implementation defines parsers for the statements.
 ***************************************************************************/


use nom::{
    character::complete::{multispace0, multispace1},
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{preceded, terminated, tuple},
    branch::alt,
    multi::many0,
    error::context,
};
use crate::sqrrlc_ast::{
    span::{LineColumn, Span},
    base::Item,
    expr::{Expr, ExprIdent},
    ty::Ty,
    stmt::*,
};
use crate::sqrrlc_parser::{
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parse expression block.
 */
impl Parser for Block {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "block statement",
            map(tuple((
                preceded(multispace0, tag("{")),
                many0(Stmt::parse),
                opt(Expr::parse),
                preceded(multispace0, tag("}")),
            )),
                |(start, mut stmts, ret, end)| {
                    if ret.is_some() {
                        stmts.push(Stmt::Expr(ret.unwrap()));
                    }
                    Block {
                        stmts: stmts,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            LineColumn::new(end.line, end.get_column() + 1),
                            input.extra
                        ),
                    }
                }
            )
        )(input)
    }
}


/**
 * Parse different types of statements.
 */
impl Parser for Stmt {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "statement",
            alt((
                map(Local::parse, |local| Stmt::Local(local)),
                map(Item::parse,  |item|  Stmt::Item(item)),
                map(terminated(Expr::parse, preceded(multispace0, tag(";"))),
                    |expr|  Stmt::Expr(expr)),
                map(Expr::parse,  |expr|  Stmt::Expr(expr)),
            ))
        )(input)
    }
}


/**
 * Parse a local variable (let binding).
 */
impl Parser for Local {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "local variable",
            map(tuple((
                preceded(multispace0, tag("let")),
                opt(preceded(multispace1, tag("mut"))),
                preceded(multispace1, ExprIdent::parse),
                preceded(multispace0, tag(":")),
                preceded(multispace0, Ty::parse),
                preceded(multispace0, tag("=")),
                preceded(multispace0, Expr::parse),
                preceded(multispace0, tag(";")),
            )),
                |(start, mutable, ident, _, ty, _, expr, end)| {
                    Local {
                        mutable: mutable.is_some(),
                        ident: ident,
                        ty: ty,
                        init: Box::new(expr),
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            LineColumn::new(end.line, end.get_column() + 1),
                            input.extra
                        ),
                    }
                }
            )
        )(input)
    }
}
