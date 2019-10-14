
/***************************************************************************
 * Base parser implementation defines parsers for the base constructs
 * i.e. a file and its items an item can be for example a function.
 ***************************************************************************/

use nom::{
    character::complete::{multispace0, multispace1},
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{delimited, preceded, pair, tuple},
    multi::{many0, separated_list},
    branch::alt,
    error::context,
};


use crate::ast::{
    span::Span,
    base::*,
    expr::{ExprBlock, ExprIdent},
};


use crate::parser::{
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parse a source file containing items such as functions.
 */
impl Parser for File {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "file",
            map(many0(Item::parse),
                |items| File {
                    items: items,
                    span: Span::new(input),
                }
            )
        )(input)
    }
}


/**
 * Parse an item can atm only be a function.
 */
impl Parser for Item {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "item",
            map(ItemFn::parse, |func| Item::Fn(func)),
        )(input)
    }
}


/**
 * Parse a function.
 */
impl Parser for ItemFn {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "function",
            map(tuple((
                preceded(multispace0, tag("fn")),
                preceded(multispace1, ExprIdent::parse),
                FnDecl::parse,
                ExprBlock::parse,
            )),
                |(_, id, decl, block)| ItemFn {
                    ident: id,
                    decl: decl,
                    block: block,
                    span: Span::new(input),
                }
            )
        )(input)
    }
}


/**
 * Parse a function delcaration, this includes
 * the input arguments and optionally output type.
 */
impl Parser for FnDecl {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "function declaration",
            map(pair(
                delimited(
                    preceded(multispace0, tag("(")),
                    separated_list(
                        preceded(multispace0, tag(",")),
                        Argument::parse
                    ),
                    preceded(multispace0, tag(")"))
                ),
                opt(pair(
                    preceded(multispace0, tag("->")),
                    Type::parse
                )),
            ),
                |(args, ret_ty)| {
                    let output;
                    match ret_ty {
                        Some(ty) => output = Some(ty.1),
                        None => output = None,
                    }
                    FnDecl {
                        inputs: args,
                        output: output,
                        span: Span::new(input),
                    }
                }
            )
        )(input)
    }
}


/**
 * Parse a function argument.
 */
impl Parser for Argument {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "argument",
            map(tuple((
                ExprIdent::parse,
                preceded(multispace0, tag(":")),
                Type::parse,
            )),
                |(id, _, ty)| Argument {
                    ident: id,
                    ty: ty,
                    span: Span::new(input),
                }
            )
        )(input)
    }
}


/**
 * Parse a type either i32 or bool.
 */
impl Parser for Type {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "type",
            alt((
                map(preceded(multispace0, tag("i32")), |s| Type::Int32{span: Span::new(s)}),
                map(preceded(multispace0, tag("bool")), |s| Type::Bool{span: Span::new(s)}),
            ))
        )(input)    
    }
}
