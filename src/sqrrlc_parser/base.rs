
/***************************************************************************
 * Base parser implementation defines parsers for the base constructs
 * i.e. a file and its items an item can be for example a function.
 ***************************************************************************/


use nom::{
    character::complete::{multispace0, multispace1},
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{preceded, pair, tuple},
    multi::separated_list,
    error::context,
    Err,
};
use crate::sqrrlc_ast::{
    span::{Span, LineColumn},
    base::*,
    expr::{ExprBlock, ExprIdent},
    ty::Ty,
};
use crate::sqrrlc_parser::{
    error::convert_error,
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parse a source file containing items such as functions.
 */
impl Parser for File {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        let mut output = input;
        let mut items = Vec::new();
        while output.fragment.len() > 0 {
            match Item::parse(output) {
                Ok((input, item)) => {
                    items.push(item);
                    let (span, _) = multispace0(input)?;
                    output = span;
                },
                Err(Err::Error(e)) => {
                    println!("{}", convert_error(&input, e));
                    break;
                },
                _ => break,
            };
        }
        Ok((output, File {
            items: items,
            span: Span::new(input),
        }))
    }
}


/**
 * Parse an item can atm only be a function.
 */
impl Parser for Item {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "item",
            map(FnItem::parse, |func| Item::Fn(func)),
        )(input)
    }
}


/**
 * Parse a function.
 */
impl Parser for FnItem {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "function",
            map(tuple((
                preceded(multispace0, tag("fn")),
                preceded(multispace1, ExprIdent::parse),
                FnDecl::parse,
                ExprBlock::parse,
            )),
                |(start, id, decl, block)| {
                    let block_clone = block.clone();
                    FnItem {
                        ident: id,
                        decl: decl,
                        block: block,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            block_clone.span.end,
                            input.extra,
                        ),
                    }
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
            map(tuple((
                preceded(multispace0, tag("(")),
                separated_list(
                    preceded(multispace0, tag(",")),
                    Argument::parse
                ),
                preceded(multispace0, tag(")")),
                opt(pair(
                    preceded(multispace0, tag("->")),
                    Ty::parse
                )),
            )),
                |(start, args, end, ret_ty)| {
                    let output;
                    let end_span;
                    match ret_ty {
                        Some(ty) => {
                            end_span = ty.1.span.end;
                            output = ty.1;
                        },
                        None => {
                            output = Ty::new();
                            end_span = LineColumn::new(end.line, end.get_column() + 1);
                        },  
                    };
                    FnDecl {
                        inputs: args,
                        output: output,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            end_span, input.extra
                        ),
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
                Ty::parse,
            )),
                |(id, _, ty)| {
                    let id_span = id.clone().span;
                    let ty_span = ty.span.clone();
                    Argument {
                        ident: id,
                        ty: ty,
                        span: Span::from_bounds(
                            id_span.start, ty_span.end, input.extra),
                    }
                }
            )
        )(input)
    }
}
