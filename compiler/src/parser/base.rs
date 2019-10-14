
/***************************************************************************
 * Base parser implementation defines parsers for the base constructs
 * i.e. a file and its items an item can be for example a function.
 ***************************************************************************/

use nom::{
    character::is_alphanumeric,
    character::complete::{alpha1, multispace0},
    bytes::complete::{take_while1, tag},
    combinator::{map, opt, peek},
    sequence::{delimited, preceded, terminated, pair, tuple},
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


impl Parser for File {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "file",
        )(input)
    }
}


impl Parser for Item {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "item",
            map(ItemFn::parse, |func| Item::Fn(func)),
        )(input)
    }
}


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
                |(_, id, decl, block)| ItemFn{
                    ident: id,
                    decl: decl,
                    block: block,
                    span: Span::new(input),
                }
            )
        )(input)
    }
}


impl Parser for FnDecl {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "function declaration",
            map(tuple((
                delimited(
                    preceded(multispace0, tag("(")),
                    Argument::parse,
                    preceded(multispace0, tag(")"))
                ),
                opt(
                    
                ),
            )),
                |(args, ret)|
        )(input)
    }
}


impl Parser for Argument {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "argument",
            map(tuple((
                ExprIdent::parse,
                preceded(multispace0, tag(":")),
                Type::parse,
            )),
                |(id, _, ty)| Argument{
                    ident: id,
                    ty: ty,
                    span: Span::new(input),
                }
            )
        )(input)
    }
}


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
