
/***************************************************************************
 * Base parser implementation defines parsers for the base constructs
 * i.e. a file and its items an item can be for example a function.
 ***************************************************************************/


use nom::{
    character::complete::{multispace0, multispace1},
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{preceded, terminated, pair, tuple},
    multi::separated_list,
    error::context,
    Err,
};
use crate::sqrrlc_ast::{
    span::{Span, LineColumn},
    base::*,
    expr::ExprIdent,
    stmt::Block,
    ty::Ty,
};
use crate::sqrrlc_parser::{
    comment::multispace_comment0,
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
        let (mut output, _) = multispace_comment0(input)?;
        let mut items = Vec::new();
        while output.fragment.len() > 0 {
            match Item::parse(output) {
                Ok((input, item)) => {
                    items.push(item);
                    let (span, _) = multispace_comment0(input)?;
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
            span: Span::new(input, input.extra),
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
                Block::parse,
            )),
                |(start, id, decl, block)| {
                    let block_clone = block.clone();
                    FnItem {
                        ident: id,
                        decl: decl,
                        block: block,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            block_clone.span.end, input.extra
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
                opt(preceded(multispace0, terminated(tag("mut"), multispace1))),
                ExprIdent::parse,
                preceded(multispace0, tag(":")),
                Ty::parse,
            )),
                |(mut_token, id, _, ty)| {
                    let start_lc = match mut_token {
                        Some(mutable) => LineColumn::new(mutable.line, mutable.get_column()),
                        None => id.span.start,
                    };
                    let end_lc = ty.span.end;
                    Argument {
                        mutable: mut_token.is_some(),
                        ident: id,
                        ty: ty,
                        span: Span::from_bounds(start_lc, end_lc, input.extra),
                    }
                }
            )
        )(input)
    }
}
