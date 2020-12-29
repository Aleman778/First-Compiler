#![allow(dead_code)]

use nom::{
    character::is_alphanumeric,
    character::complete::{alpha1, digit1, multispace0, multispace1},
    bytes::complete::{take, take_while, take_while1, take_until, tag},
    combinator::{peek, map, opt},
    sequence::{preceded, terminated, pair, tuple},
    branch::alt,
    multi::{many0, separated_list},
    error::context,
    Err,
};
use nom_locate::LocatedSpanEx;
use std::collections::HashMap;
use crate::error::*;
use crate::ast::*;

/**
 * Type alias of LocatedSpanEx for convenience.
 * First string is the input string, other string is
 * the filename that is being parsed.
 */
pub type ParseSpan<'a> = LocatedSpanEx<&'a str, u16>;

/**
 * Type aliased IResult from std::Result.
 */
pub type IResult<I, O> = Result<(I, O), Err<ParseError>>;

/**
 * Parse error struct holds information about the error and location
 */
pub struct ParseError {
    errors: Vec<Verbose>,
}

/**
 * Gives context to an error e.g. location, kind, file etc.
 */
pub struct Verbose {
    span: Span,
    kind: ErrorKind,
}

/**
 * Parse a source file containing items such as functions.
 */
pub fn parse_file(source: String, filename: String) -> File {
    // Calculate the byte position of each line in the source.
    let mut curr_pos = 0;
    let mut lines = vec![0];
    let mut remaining: &str = &source;
    loop {
        match remaining.find("\n") {
            Some(pos) => {
                curr_pos += (pos as u32) + 1;
                lines.push(curr_pos);
                remaining = remaining.split_at(pos + 1).1;
            },
            None => break,
        }
    }

    if lines.last() != Some(&(source.len() as u32)) {
        lines.push(source.len() as u32);
    }

    // Parse the source file
    let input = ParseSpan::new_extra(&source, 0); // TODO(alexander): temporary 0 should be some file id maybe.
    let mut output = match multispace_comment0(input) {
        Ok((out, _)) => out,
        _ => input,
    };
    let mut items = Vec::new();
    while output.fragment.len() > 0 {
        match parse_item(output) {
            Ok((input, item)) => {
                items.push(item);
                output = match multispace_comment0(input) {
                    Ok((out, _)) => out,
                    _ => output,
                };
            },
            Err(Err::Error(error)) => {
                parse_error(error, &source, &filename, &lines);
                break;
            },
            _ => break,
        };
    }

    let span = Span::from_parse_span(input);
    let imported_files = HashMap::new();

    File { source, filename, items, span, lines, imported_files }
}

pub fn parse_item(input: ParseSpan) -> IResult<ParseSpan, Item> {
    context(
        "item",
        alt((
            map(parse_fn_item, |func| Item::Fn(func)),
            map(parse_foreign_fn_item, |func| Item::ForeignFn(func)),
            map(parse_foreign_mod_item, |module| Item::ForeignMod(module)),
        ))
    )(input)
}

pub fn parse_foreign_item(input: ParseSpan) -> IResult<ParseSpan, Item> {
    context(
        "item",
        map(parse_foreign_fn_item, |func| Item::ForeignFn(func))
    )(input)
}

pub fn parse_fn_item(input: ParseSpan) -> IResult<ParseSpan, FnItem> {
    context(
        "function",
        map(tuple((
            preceded(multispace0, tag("fn")),
            preceded(multispace1, parse_ident_expr),
            parse_fn_decl,
            parse_block,
        )),
            |(start, id, decl, block)| {
                let block_span = block.span;
                FnItem {
                    ident: id,
                    decl: decl,
                    block: block,
                    span: Span::combine(
                        Span::from_parse_span(start),
                        block_span,
                    ),
                }
            }
        )
    )(input)
}

pub fn parse_foreign_fn_item(input: ParseSpan) -> IResult<ParseSpan, ForeignFnItem> {
    context(
        "foreign function",
        map(tuple((
            preceded(multispace0, tag("fn")),
            preceded(multispace1, parse_ident_expr),
            parse_fn_decl,
            preceded(multispace0, tag(";")),
        )),
            |(start, id, decl, semi)| {
                ForeignFnItem {
                    ident: id,
                    decl: decl,
                    span: Span::combine(
                        Span::from_parse_span(start),
                        Span::from_parse_span(semi),
                    ),
                }
            }
        )
    )(input)
}

pub fn parse_foreign_mod_item(input: ParseSpan) -> IResult<ParseSpan, ForeignModItem> {
    context(
        "extern",
        map(tuple((
            preceded(multispace0, tag("extern")),
            opt(preceded(multispace0, parse_string)),
            preceded(multispace0, tag("{")),
            many0(preceded(multispace_comment0, parse_foreign_item)),
            preceded(multispace0, tag("}")),
        )),
            |(start, abi_string, _, items, end)| {
                ForeignModItem {
                    abi: abi_string.map(|(abi, _)| abi),
                    items,
                    span: Span::combine(
                        Span::from_parse_span(start),
                        Span::from_parse_span(end),
                    ),
                }
            }
        )
    )(input)
}

pub fn parse_fn_decl(input: ParseSpan) -> IResult<ParseSpan, FnDecl> {
    context(
        "function declaration",
        map(tuple((
            preceded(multispace0, tag("(")),
            separated_list(
                preceded(multispace0, tag(",")),
                parse_argument
            ),
            preceded(multispace0, tag(")")),
            opt(pair(
                preceded(multispace0, tag("->")),
                parse_ty
            )),
        )),
            |(start, args, end, ret_ty)| {
                let mut output;
                let end_span;
                match ret_ty {
                    Some(ty) => {
                        end_span = ty.1.span;
                        output = ty.1;
                    },
                    None => {
                        output = Ty::default();
                        end_span = Span::from_parse_span(end);
                        output.span = Span::combine(end_span, end_span);
                    },
                };
                FnDecl {
                    inputs: args,
                    output: output,
                    span: Span::combine(Span::from_parse_span(start), end_span),
                }
            }
        )
    )(input)
}

pub fn parse_argument(input: ParseSpan) -> IResult<ParseSpan, Argument> {
    context(
        "argument",
        map(tuple((
            opt(preceded(multispace0, terminated(tag("mut"), multispace1))),
            parse_ident_expr,
            preceded(multispace0, tag(":")),
            parse_ty,
        )),
            |(mut_token, id, _, ty)| {
                let start = match mut_token {
                    Some(mutable) => Span::from_parse_span(mutable),
                    None => id.span,
                };
                let end = ty.span;
                Argument {
                    mutable: mut_token.is_some(),
                    ident: id,
                    ty: ty,
                    span: Span::combine(start, end)
                }
            }
        )
    )(input)
}

pub fn parse_block(input: ParseSpan) -> IResult<ParseSpan, Block> {
    context(
        "block statement",
        map(tuple((
            preceded(multispace_comment0, tag("{")),
            many0(preceded(multispace_comment0, parse_stmt)),
            preceded(multispace_comment0, tag("}")),
        )),
            |(start, stmts, end)| {
                Block {
                    stmts: stmts,
                    span: Span::combine(
                        Span::from_parse_span(start),
                        Span::from_parse_span(end),
                    ),
                }
            }
        )
    )(input)
}

pub fn parse_stmt(input: ParseSpan) -> IResult<ParseSpan, Stmt> {
    context(
        "statement",
        alt((
            map(parse_local_stmt, |local| Stmt::Local(local)),
            map(parse_item,  |item|  Stmt::Item(item)),
            map(terminated(parse_expr, preceded(multispace0, tag(";"))),
                |expr|  Stmt::Semi(expr)),
            map(parse_expr,  |expr|  Stmt::Expr(expr)),
        ))
    )(input)
}

pub fn parse_local_stmt(input: ParseSpan) -> IResult<ParseSpan, Local> {
    context(
        "local variable",
        map(tuple((
            preceded(multispace0, tag("let")),
            opt(preceded(multispace1, tag("mut"))),
            preceded(multispace1, parse_ident_expr),
            preceded(multispace0, tag(":")),
            preceded(multispace0, parse_ty),
            opt(pair(
                preceded(multispace0, tag("=")),
                preceded(multispace0, parse_expr)
            )),
            preceded(multispace0, tag(";")),
        )),
            |(start, mutable, ident, _, ty, init, end)| {
                let init = match init {
                    Some((_, expr)) => Some(expr),
                    None => None,
                };
                Local {
                    mutable: mutable.is_some(),
                    ident: ident,
                    ty: ty,
                    init: Box::new(init),
                    span: Span::combine(
                        Span::from_parse_span(start),
                        Span::from_parse_span(end),
                    ),
                }
            }
        )
    )(input)
}

pub fn parse_ty(input: ParseSpan) -> IResult<ParseSpan, Ty> {
    context(
        "type",
        map(parse_ty_kind, |(kind, span)| Ty::new(kind, span))
    )(input)
}

fn parse_ty_kind(input: ParseSpan) -> IResult<ParseSpan, (TyKind, Span)> {
    alt((
        map(preceded(multispace0, tag("i32")), |s| (TyKind::Int, Span::from_parse_span(s))),
        map(preceded(multispace0, tag("bool")), |s| (TyKind::Bool, Span::from_parse_span(s))),
        map(preceded(multispace0, parse_ty_ref), |r| (TyKind::Ref(r.0), r.1)),
    ))(input)
}

fn parse_ty_ref(input: ParseSpan) -> IResult<ParseSpan, (TypeRef, Span)> {
    context(
        "type reference",
        map(tuple((
            preceded(multispace0, tag("&")),
            opt(preceded(multispace0, terminated(tag("mut"), multispace1))),
            preceded(multispace0, parse_ty),
        )),
            |(amp, mut_token, elem)| {
                let elem_span = elem.span;
                (TypeRef{mutable: mut_token.is_some(), elem: Box::new(elem)},
                 Span::combine(Span::from_parse_span(amp), elem_span))
            }
        )
    )(input)
}

pub fn parse_binop(input: ParseSpan) -> IResult<ParseSpan, BinOp> {
    context(
        "operator",
        preceded(multispace0, alt((
            map(tag("+"),  |_| BinOp::Add),
            map(tag("-"),  |_| BinOp::Sub),
            map(tag("**"), |_| BinOp::Pow),
            map(tag("*"),  |_| BinOp::Mul),
            map(tag("/"),  |_| BinOp::Div),
            map(tag("%"),  |_| BinOp::Mod),
            map(tag("&&"), |_| BinOp::And),
            map(tag("||"), |_| BinOp::Or),
            map(tag("=="), |_| BinOp::Eq),
            map(tag("!="), |_| BinOp::Ne),
            map(tag("<="), |_| BinOp::Le),
            map(tag(">="), |_| BinOp::Ge),
            map(tag("<"),  |_| BinOp::Lt),
            map(tag(">"),  |_| BinOp::Gt),
        )))
    )(input)
}

pub fn parse_unop(input: ParseSpan) -> IResult<ParseSpan, UnOp> {
    context(
        "operator",
        preceded(multispace0, alt((
            map(tag("-"),  |_| UnOp::Neg),
            map(tag("!"),  |_| UnOp::Not),
            map(tag("*"),  |_| UnOp::Deref),
        )))
    )(input)
}

pub fn parse_expr(input: ParseSpan) -> IResult<ParseSpan, Expr> {
    context(
        "expression",
        alt((
            map(parse_assign_expr,    |expr_assign|   Expr::Assign(expr_assign)),
            map(parse_if_expr,        |expr_if|       Expr::If(expr_if)),
            map(parse_while_expr,     |expr_while|    Expr::While(expr_while)),
            map(parse_block_expr,     |expr_block|    Expr::Block(expr_block)),
            map(parse_return_expr,    |expr_return|   Expr::Return(expr_return)),
            map(parse_break_expr,     |expr_break|    Expr::Break(expr_break)),
            map(parse_continue_expr,  |expr_continue| Expr::Continue(expr_continue)),
            map(parse_call_expr,      |expr_call|     Expr::Call(expr_call)),
            preceded(multispace0, parse_binary_expr),
        ))
    )(input)
}

pub fn parse_expr_atom(input: ParseSpan) -> IResult<ParseSpan, Expr> {
    context(
        "expression",
        alt((
            map(parse_lit_expr,       |literal|  Expr::Lit(literal)),
            map(parse_paren_expr,     |expr|     Expr::Paren(expr)),
            map(parse_call_expr,      |call|     Expr::Call(call)),
            map(parse_ident_expr,     |ident|    Expr::Ident(ident)),
            map(parse_unary_expr,     |unary|    Expr::Unary(unary)),
            map(parse_reference_expr, |expr_ref| Expr::Reference(expr_ref)),
        ))
    )(input)
}

pub fn parse_assign_expr(input: ParseSpan) -> IResult<ParseSpan, ExprAssign> {
    context(
        "assignment",
        preceded(
            multispace0,
            map(tuple((
                parse_expr_atom,
                preceded(multispace0, tag("=")),
                parse_expr,
                preceded(multispace0, peek(tag(";"))),
            )),
                |(left, _, right, end)| {
                    let left_span = left.get_span();
                    ExprAssign {
                        left: Box::new(left),
                        right: Box::new(right),
                        span: Span::combine(left_span, Span::from_parse_span(end)),
                    }
                }
            )
        )
    )(input)
}

pub fn parse_binary_expr(input: ParseSpan) -> IResult<ParseSpan, Expr> {
    parse_binary_precedence_climb(input, 1)
}

pub fn parse_binary_precedence_climb(input: ParseSpan, min_prec: u8) -> IResult<ParseSpan, Expr> {
    let (mut output, mut expr_lhs) = parse_expr_atom(input)?;
    loop {
        match peek(parse_binop)(output) {
            Ok((_, operator)) => {
                let (prec, assoc) = operator.get_prec();
                if prec < min_prec {
                    break;
                }
                let next_min_prec = match assoc {
                    Assoc::Left => prec + 1,
                    Assoc::Right => prec,
                };
                let (span, operator) = parse_binop(output)?;
                let (span, _) = multispace0(span)?;
                let (span, expr_rhs) = parse_binary_precedence_climb(span, next_min_prec)?;
                output = span;
                expr_lhs = Expr::Binary(ExprBinary {
                    left: Box::new(expr_lhs),
                    op: operator,
                    right: Box::new(expr_rhs),
                    span: Span::between(
                        Span::from_parse_span(input),
                        Span::from_parse_span(output),
                    ),
                });
            },
            _ => break,
        }
    }
    Ok((output, expr_lhs))
}

pub fn parse_block_expr(input: ParseSpan) -> IResult<ParseSpan, ExprBlock> {
    map(parse_block, |block| {
        let span = block.span;
        ExprBlock { block, span }
    })(input)
}

pub fn parse_break_expr(input: ParseSpan) -> IResult<ParseSpan, ExprBreak> {
    context(
        "break",
        map(pair(
            preceded(multispace0, tag("break")),
            preceded(multispace0, peek(tag(";")))
        ),
            |(start, end) : (ParseSpan, ParseSpan)| ExprBreak {
                span: Span::combine(
                    Span::from_parse_span(start),
                    Span::from_parse_span(end),
                ),
            }
        )
    )(input)
}

fn parse_call_expr(input: ParseSpan) -> IResult<ParseSpan, ExprCall> {
    context(
        "function call",
        map(tuple((
            parse_ident_expr,
            preceded(multispace0, tag("(")),
            separated_list(preceded(multispace0, tag(",")), parse_expr),
            preceded(multispace0, tag(")")),
        )),
            |(id, _, args, end)| {
                let rid = id.clone();
                ExprCall {
                    ident: id,
                    args: args,
                    span: Span::combine(
                        rid.span,
                        Span::from_parse_span(end),
                    ),
                }
            }
        )
    )(input)
}

pub fn parse_continue_expr(input: ParseSpan) -> IResult<ParseSpan, ExprContinue> {
    context(
        "continue",
        map(pair(
            preceded(multispace0, tag("continue")),
            preceded(multispace0, peek(tag(";")))
        ),
            |(start, end) : (ParseSpan, ParseSpan)| ExprContinue {
                span: Span::combine(
                    Span::from_parse_span(start),
                    Span::from_parse_span(end)
                ),
            }
        )
    )(input)
}

pub fn parse_ident_expr(input: ParseSpan) -> IResult<ParseSpan, ExprIdent> {
    context(
        "identifier",
        preceded(multispace0, map(
            pair(
                peek(alt((alpha1, tag("_")))),
                take_while1(|c: char| is_alphanumeric(c as u8) || c == '_')),
            |(_, s): (ParseSpan, ParseSpan)| ExprIdent {
                sym: intern_string(s.fragment),
                span: Span::from_parse_span(s)
            })
        )
    )(input)
}

pub fn parse_if_expr(input: ParseSpan) -> IResult<ParseSpan, ExprIf> {
    context(
        "if statement",
        map(tuple((
            preceded(multispace0, tag("if")),
            preceded(multispace1, parse_expr),
            preceded(multispace0, parse_block),
            opt(preceded(
                pair(multispace0, tag("else")),
                preceded(multispace0, parse_block))
            ),
        )),
            |(start, cond, then_block, else_block)| {
                let end = match else_block.clone() {
                    Some(block) => block,
                    None => then_block.clone(),
                };
                ExprIf {
                    cond: Box::new(cond),
                    then_block: then_block,
                    else_block: else_block,
                    span: Span::combine(
                        Span::from_parse_span(start),
                        end.span
                    ),
                }
            }
        )
    )(input)
}

pub fn parse_lit_expr(input: ParseSpan) -> IResult<ParseSpan, ExprLit> {
    context(
        "literal",
        alt((
            map(parse_int,  |(val, span)| ExprLit { lit: Lit::Int (val), span: span }),
            map(parse_bool, |(val, span)| ExprLit { lit: Lit::Bool(val), span: span }),
        )),
    )(input)
}


pub fn parse_paren_expr(input: ParseSpan) -> IResult<ParseSpan, ExprParen> {
    map(tuple((
        preceded(multispace0, tag("(")),
        parse_expr,
        preceded(multispace0, tag(")"))
    )),
        |(start, expr, end)| ExprParen {
            expr: Box::new(expr),
            span: Span::combine(
                Span::from_parse_span(start),
                Span::from_parse_span(end)
            ),
        }
    )(input)
}

pub fn parse_reference_expr(input: ParseSpan) -> IResult<ParseSpan, ExprReference> {
    map(tuple((
        preceded(multispace0, tag("&")),
        opt(preceded(multispace0, terminated(tag("mut"), multispace1))),
        preceded(multispace0, parse_expr_atom),
    )),
        |(amp, mut_token, expr)| {
            let expr_span = expr.get_span();
            ExprReference {
                mutable: mut_token.is_some(),
                expr: Box::new(expr),
                span: Span::combine(Span::from_parse_span(amp), expr_span),
            }
        }
    )(input)
}

pub fn parse_return_expr(input: ParseSpan) -> IResult<ParseSpan, ExprReturn> {
    context(
        "return",
        map(tuple((
            preceded(multispace0, tag("return")),
            opt(preceded(multispace1, parse_expr )),
            preceded(multispace0, peek(tag(";"))),
        )),
            |(start, expr, end)| ExprReturn {
                expr: Box::new(expr),
                span: Span::combine(
                    Span::from_parse_span(start),
                    Span::from_parse_span(end)
                ),
            }
        )
    )(input)
}

pub fn parse_unary_expr(input: ParseSpan) -> IResult<ParseSpan, ExprUnary> {
    let (input, _) = multispace0(input)?;
    let (span, op) = parse_unop(input)?;
    let (span, expr) = parse_binary_precedence_climb(span, op.get_prec().0)?;
    Ok((span, ExprUnary {
        op: op,
        expr: Box::new(expr),
        span: Span::between(Span::from_parse_span(input), Span::from_parse_span(span)),
    }))
}

pub fn parse_while_expr(input: ParseSpan) -> IResult<ParseSpan, ExprWhile> {
    context(
        "while loop",
        map(tuple((
            preceded(multispace0, tag("while")),
            preceded(multispace1, parse_expr),
            preceded(multispace0, parse_block)
        )),
            |(start, cond, block)| {
                let block_span = block.span;
                ExprWhile {
                    cond: Box::new(cond),
                    block: block,
                    span: Span::combine(Span::from_parse_span(start), block_span),
                }
            }
        )
    )(input)
}

pub fn parse_int(input: ParseSpan) -> IResult<ParseSpan, (i32, Span)> {
    let (input, digits) = preceded(multispace0, digit1)(input)?;
    match digits.fragment.parse::<i32>() {
        Ok(n) => Ok((input, (n, Span::from_parse_span(digits)))),
        Err(e) => Err(Err::Error(ParseError::new(digits, ErrorKind::ParseIntError(e)))),
    }
}

pub fn parse_bool(input: ParseSpan) -> IResult<ParseSpan, (bool, Span)> {
    preceded(multispace0, alt((
        map(tag("true"),  |s| (true,  Span::from_parse_span(s))),
        map(tag("false"), |s| (false, Span::from_parse_span(s))),
    )))(input)
}

pub fn parse_string(input: ParseSpan) -> IResult<ParseSpan, (String, Span)> {
    preceded(multispace0, map(tuple((
        tag("\""),
        take_while(|c: char| c != '"'),
        tag("\""),
    )), |(left, s, right): (ParseSpan, ParseSpan, ParseSpan)| {
        (s.fragment.to_string(), Span::combine(
            Span::from_parse_span(left),
            Span::from_parse_span(right)))
    }))(input)
}

pub fn multispace_comment0(input: ParseSpan) -> IResult<ParseSpan, ()> {
    map(
        many0(
            alt((
                map(multispace1, |_| ()),
                any_comment,
            ))
        ), |_| ()
    )(input)
}

pub fn any_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    alt((
        doc_comment,
        block_doc_comment,
        line_comment,
        block_comment,
    ))(input)
}

pub fn line_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    context(
        "comment",
        map(pair(tag("//"), take_until("\n")), |_| ())
    )(input)
}

pub fn block_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    let mut input = tag("/*")(input)?.0;
    loop {
        let next: IResult<ParseSpan, ParseSpan> = take(1usize)(input);
        match next {
            Ok((inpt, _)) => input = inpt,
            Err(_) => return Err(Err::Error(ParseError::new(
                input, ErrorKind::Context("unterminated block comment")))),
        };
        let new: IResult<ParseSpan, ParseSpan> = tag("/*")(input);
        let end: IResult<ParseSpan, ParseSpan> = tag("*/")(input);
        if new.is_ok() {
            input = block_comment(input)?.0;
        } else if end.is_ok() {
            return map(tag("*/"), |_| ())(input);
        }
    }
}

fn doc_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    context(
        "doc-comment",
        map(pair(tag("///"), take_until("\n")), |_| ())
    )(input)
}

fn block_doc_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    let mut input = tag("/**")(input)?.0;
    loop {
        let next: IResult<ParseSpan, ParseSpan> = take_until("\n")(input);
        match next {
            Ok((inpt, _)) => input = inpt,
            Err(_) => return Err(Err::Error(ParseError::new(
                input, ErrorKind::Context("unterminated block doc-comment")))),
        };
        input = multispace0(input)?.0;
        let middle: IResult<ParseSpan, ParseSpan> = tag("*")(input);
        let new: IResult<ParseSpan, ParseSpan> = tag("/**")(input);
        let end: IResult<ParseSpan, ParseSpan> = tag("*/")(input);
        if middle.is_ok() {
            input = middle?.0;
        } else if new.is_ok() {
            input = block_doc_comment(input)?.0;
        } else if end.is_ok() {
            return Ok((input, ()));
        }
    }
}

impl<'a> ParseError {
    pub fn new(input: ParseSpan<'a>, kind: ErrorKind) -> Self {
        ParseError{
            errors: vec![Verbose{
                span: Span::from_parse_span(input),
                kind: kind,
            }],
        }
    }

    pub fn append(input: ParseSpan<'a>, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push(Verbose {
            span: Span::from_parse_span(input),
            kind: kind,
        });
        other
    }

    fn is_important(&self) -> bool {
        match self.errors.last() {
            Some(verbose) => {
                match verbose.kind {
                    ErrorKind::ParseIntError(_) => true,
                    _ => false,
                }

            },
            None => false,
        }
    }
}

/**
 * Implementation of nom ParseErrors for my custom ParseError
 */
impl<'a> nom::error::ParseError<ParseSpan<'a>> for ParseError {
    fn from_error_kind(input: ParseSpan<'a>, kind: nom::error::ErrorKind) -> Self {
        ParseError{
            errors: vec![Verbose{
                span: Span::from_parse_span(input),
                kind: ErrorKind::Nom(kind),
            }],
        }
    }

    fn append(input: ParseSpan<'a>, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.errors.push(Verbose{
            span: Span::from_parse_span(input),
            kind: ErrorKind::Nom(kind),
        });
        other
    }

    fn from_char(input: ParseSpan<'a>, c: char) -> Self {
        ParseError{
            errors: vec![Verbose{
                span: Span::from_parse_span(input),
                kind: ErrorKind::Char(c),
            }],
        }
    }

    fn or(self, other: Self) -> Self {
        let sim = self.is_important();
        let oim = other.is_important();
        if sim && !oim {
            self
        } else if !sim && oim {
            other
        } else if self.errors.len() > other.errors.len() {
            self
        } else {
            other
        }
    }

    fn add_context(input: ParseSpan<'a>, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push(Verbose{
            span: Span::from_parse_span(input),
            kind: ErrorKind::Context(ctx),
        });
        other
    }
}

/**
 * Prints the given errors from the parser using the source, current filename and line number byte offsets.
 */
fn parse_error<'a>(error: ParseError, source: &str, filename: &str, lines: &Vec<u32>) {
    for err in error.errors {
        let error_msg = match &err.kind {
            ErrorKind::ParseIntError(e) => e.to_string(),
            ErrorKind::Nom(e) => (*e).description().to_string(),
            ErrorKind::Char(e) => format!("{}", *e),
            ErrorKind::Context(e) => {
                let beg = err.span.base as usize;
                let end = err.span.base as usize + err.span.len as usize;
                format!("expected `{}`, found `{}`", e, &source[beg..end])
            },
        };
        
        print_error_msg(&create_error_msg_from_span(ErrorLevel::Error,
                                                   lines,
                                                   err.span,
                                                   filename,
                                                   source,
                                                   &error_msg,
                                                   ""))
    }
}

/**
 * Error kind enum defines different types of parse errors.
 */
#[derive(Debug)]
pub enum ErrorKind {
    /// Failed to parse an int e.g. overflow.
    ParseIntError(std::num::ParseIntError),
    /// Error kind given by various nom parsers.
    Nom(nom::error::ErrorKind),
    /// Indicates which character was expected by the char function
    Char(char),
    /// Static string added by the `context` function
    Context(&'static str),
}
