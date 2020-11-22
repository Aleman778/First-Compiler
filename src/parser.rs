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
type IResult<I, O> = Result<(I, O), Err<ParseError>>;

/**
 * Parse error struct holds information about the error and location
 */
struct ParseError {
    errors: Vec<Verbose>,
}

/**
 * Gives context to an error e.g. location, kind, file etc.
 */
struct Verbose {
    span: Span,
    kind: ErrorKind,
}

/**
 * Parser trait defines a generic parser that should
 * be implemented by each structure of the AST.
 */
pub trait Parser: Sized {
    fn parse<'a>(input: ParseSpan) -> IResult<ParseSpan, Self>;
}

/**
 * Parse a source file containing items such as functions.
 */
pub fn parse_file(source: String, filename: String) -> File {
    // Calculate the byte position of each line in the source.
    let end_pos = source.len() as u32;
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
    };
    let mut items = Vec::new();
    while output.fragment.len() > 0 {
        match Item::parse(output) {
            Ok((input, item)) => {
                items.push(item);
                output = match multispace_comment0(input) {
                    Ok((out, _)) => out,
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

/**
 * Parse an item can atm only be a function.
 */
impl Parser for Item {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "item",
            alt((
                map(FnItem::parse, |func| Item::Fn(func)),
                map(ForeignModItem::parse, |module| Item::ForeignMod(module)),
            ))
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
                    FnItem {
                        ident: id,
                        decl: decl,
                        block: block,
                        span: Span::combine(
                            Span::from_parse_span(start),
                            block.span,
                        ),
                    }
                }
            )
        )(input)
    }
}

impl Parser for ForeignFnItem {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "foreign function",
            map(tuple((
                preceded(multispace0, tag("fn")),
                preceded(multispace1, ExprIdent::parse),
                FnDecl::parse,
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
}

impl Parser for ForeignModItem {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "extern",
            map(tuple((
                preceded(multispace0, tag("extern")),
                opt(preceded(multispace0, LitStr::parse)),
                preceded(multispace0, tag("{")),
                many0(preceded(multispace_comment0, ForeignFnItem::parse)),
                preceded(multispace0, tag("}")),
            )),
                |(start, abi, _, items, end)| {
                    ForeignModItem {
                        abi,
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
                    let mut output;
                    let end_span;
                    match ret_ty {
                        Some(ty) => {
                            end_span = ty.1.span;
                            output = ty.1;
                        },
                        None => {
                            output = Ty::new();
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
}

/**
 * Parse expression block.
 */
impl Parser for Block {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "block statement",
            map(tuple((
                preceded(multispace_comment0, tag("{")),
                many0(preceded(multispace_comment0, Stmt::parse)),
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
                    |expr|  Stmt::Semi(expr)),
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
                opt(pair(
                    preceded(multispace0, tag("=")),
                    preceded(multispace0, Expr::parse)
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
}

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
            map(preceded(multispace0, tag("i32")), |s| (TyKind::Int(IntTy::I32), Span::from_parse_span(s))),
            map(preceded(multispace0, tag("i64")), |s| (TyKind::Int(IntTy::I64), Span::from_parse_span(s))),
            map(preceded(multispace0, tag("bool")), |s| (TyKind::Bool, Span::from_parse_span(s))),
            map(preceded(multispace0, TypeRef::parse), |r| (TyKind::Ref(r.0), r.1)),
        ))(input)
    }
}

/**
 * Parse a type reference or mutable reference.
 */
impl TypeRef {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, (Self, Span)> {
        context(
            "type reference",
            map(tuple((
                preceded(multispace0, tag("&")),
                opt(preceded(multispace0, terminated(tag("mut"), multispace1))),
                preceded(multispace0, Ty::parse),
            )),
                |(amp, mut_token, elem)| {
                    let elem_span = elem.span;
                    (TypeRef{mutable: mut_token.is_some(), elem: Box::new(elem)},
                     Span::combine(Span::from_parse_span(amp), elem_span))
                }
            )
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
            preceded(multispace0, alt((
                map(tag("+"),  |s| BinOp::Add{span: Span::from_parse_span(s)}),
                map(tag("-"),  |s| BinOp::Sub{span: Span::from_parse_span(s)}),
                map(tag("**"), |s| BinOp::Pow{span: Span::from_parse_span(s)}),
                map(tag("*"),  |s| BinOp::Mul{span: Span::from_parse_span(s)}),
                map(tag("/"),  |s| BinOp::Div{span: Span::from_parse_span(s)}),
                map(tag("%"),  |s| BinOp::Mod{span: Span::from_parse_span(s)}),
                map(tag("&&"), |s| BinOp::And{span: Span::from_parse_span(s)}),
                map(tag("||"), |s| BinOp::Or {span: Span::from_parse_span(s)}),
                map(tag("=="), |s| BinOp::Eq {span: Span::from_parse_span(s)}),
                map(tag("!="), |s| BinOp::Ne {span: Span::from_parse_span(s)}),
                map(tag("<="), |s| BinOp::Le {span: Span::from_parse_span(s)}),
                map(tag(">="), |s| BinOp::Ge {span: Span::from_parse_span(s)}),
                map(tag("<"),  |s| BinOp::Lt {span: Span::from_parse_span(s)}),
                map(tag(">"),  |s| BinOp::Gt {span: Span::from_parse_span(s)}),
            )))
        )(input)
    }
}

impl Parser for UnOp {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "operator",
            preceded(multispace0, alt((
                map(tag("-"),  |s| UnOp::Neg  {span: Span::from_parse_span(s)}),
                map(tag("!"),  |s| UnOp::Not  {span: Span::from_parse_span(s)}),
                map(tag("*"),  |s| UnOp::Deref{span: Span::from_parse_span(s)}),
            )))
        )(input)
    }
}

/**
 * Parse an expression, for mathematical expressions use parse_math instead.
 */
impl Parser for Expr {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "expression",
            alt((
                map(ExprAssign::parse,    |expr_assign|   Expr::Assign(expr_assign)),
                map(ExprIf::parse,        |expr_if|       Expr::If(expr_if)),
                map(ExprWhile::parse,     |expr_while|    Expr::While(expr_while)),
                map(ExprBlock::parse,     |expr_block|    Expr::Block(expr_block)),
                map(ExprReturn::parse,    |expr_return|   Expr::Return(expr_return)),
                map(ExprBreak::parse,     |expr_break|    Expr::Break(expr_break)),
                map(ExprContinue::parse,  |expr_continue| Expr::Continue(expr_continue)),
                map(ExprCall::parse,      |expr_call|     Expr::Call(expr_call)),
                Expr::parse_math
            ))
        )(input)
    }
}

/**
 * Implementation of expression to separate parsing of standard
 * non-mathematical expressions  mathematical and also atom expressions.
 */
impl Expr {
    /**
     * Parse a mathematical expression. Uses precedence climbing
     * algorithm for parsing into a mathematically correct expression.
     */
    pub fn parse_math(input: ParseSpan) -> IResult<ParseSpan, Self> {
        preceded(multispace0, ExprBinary::parse)(input)
    }

    /**
     * Parse an atom i.e. literal, parenthesized expression, function call,
     * identifier or unary operation.
     */
    pub fn parse_atom(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "expression",
            alt((
                map(ExprLit::parse,       |literal|  Expr::Lit(literal)),
                map(ExprParen::parse,     |expr|     Expr::Paren(expr)),
                map(ExprCall::parse_term, |call|     Expr::Call(call)),
                map(ExprIdent::parse,     |ident|    Expr::Ident(ident)),
                map(ExprUnary::parse,     |unary|    Expr::Unary(unary)),
                map(ExprReference::parse, |expr_ref| Expr::Reference(expr_ref)),
            ))
        )(input)
    }
}

/**
 * Parse mutable variable assignment.
 */
impl Parser for ExprAssign {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "assignment",
            preceded(
                multispace0,
                map(tuple((
                    Expr::parse_atom,
                    preceded(multispace0, tag("=")),
                    Expr::parse,
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
}

/**
 * Implement precedence climbing algorithm
 */
impl ExprBinary {
    /**
     * Parse a binary operation.
     */
    pub fn parse(input: ParseSpan) -> IResult<ParseSpan, Expr> {
        ExprBinary::climb(input, 1)
    }

    /**
     * Parse a binary operation using precedence climbing algorithm.
     */
    fn climb(input: ParseSpan, min_prec: u8) -> IResult<ParseSpan, Expr> {
        let (mut output, mut expr_lhs) = Expr::parse_atom(input)?;
        loop {
            match peek(BinOp::parse)(output) {
                Ok((_, operator)) => {
                    let (prec, assoc) = operator.get_prec();
                    if prec < min_prec {
                        break;
                    }
                    let next_min_prec = match assoc {
                        Assoc::Left => prec + 1,
                        Assoc::Right => prec,
                    };
                    let (span, operator) = BinOp::parse(output)?;
                    let (span, _) = multispace0(span)?;
                    let (span, expr_rhs) = ExprBinary::climb(span, next_min_prec)?;
                    output = span;
                    expr_lhs = Expr::Binary(ExprBinary {
                        left: Box::new(expr_lhs),
                        op: operator,
                        right: Box::new(expr_rhs),
                        span: Span::combine(
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
}

/**
 * Parse block expression.
 */
impl Parser for ExprBlock {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        map(
            Block::parse, |block| {
                let span = block.span;
                ExprBlock{block, span}
            }
        )(input)
    }
}

/**
 * Parse break keyword.
 */
impl Parser for ExprBreak {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
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
}

/**
 * Parse function call.
 * Used for parsing expressions.
 */
impl Parser for ExprCall {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        map(pair(
            ExprCall::parse_term,
            preceded(multispace0, peek(tag(";")))
        ),
            |(mut call, end)| {
                call.span = Span::from_parse_span(end);
                call
            }
        )(input)
    }
}

/**
 * Parse function call as terminal.
 * Used for parsing atoms.
 */
impl ExprCall {
    fn parse_term(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "function call",
            map(tuple((
                ExprIdent::parse,
                preceded(multispace0, tag("(")),
                separated_list(preceded(multispace0, tag(",")), Expr::parse),
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
}

/**
 * Parse continue keyword.
 */
impl Parser for ExprContinue {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
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
}

/**
 * Parse an identifier e.g. test_id.
 */
impl Parser for ExprIdent {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "identifier",
            preceded(multispace0, map(pair(
                peek(alt((alpha1, tag("_")))),
                take_while1(|c: char| is_alphanumeric(c as u8) || c == '_')),
                |(_, s): (ParseSpan, ParseSpan)| ExprIdent {
                    to_string: s.fragment.to_string(),
                    span: Span::from_parse_span(s)
                })
            )
        )(input)
    }
}

/**
 * Parse an if statement.
 */
impl Parser for ExprIf  {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "if statement",
            map(tuple((
                preceded(multispace0, tag("if")),
                preceded(multispace1, Expr::parse),
                preceded(multispace0, Block::parse),
                opt(preceded(
                    pair(multispace0, tag("else")),
                    preceded(multispace0, Block::parse))
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
}

/**
 * Parse a literal expression.
 */
impl Parser for ExprLit {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "literal",
            alt((
                map(LitInt::parse, |literal| {
                    let lit = literal.clone();
                    ExprLit{lit: Lit::Int(literal), span: lit.span}
                }),
                map(LitBool::parse, |literal| {
                    let lit = literal.clone();
                    ExprLit{lit: Lit::Bool(literal), span: lit.span}
                }),
            )),
        )(input)
    }
}

/**
 * Parse a parenthesized expression.
 */
impl Parser for ExprParen {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        map(tuple((
            preceded(multispace0, tag("(")),
            Expr::parse,
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
}

/**
 * Parse a reference expression.
 */
impl Parser for ExprReference {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        map(tuple((
            preceded(multispace0, tag("&")),
            opt(preceded(multispace0, terminated(tag("mut"), multispace1))),
            preceded(multispace0, Expr::parse_atom),
        )),
            |(amp, mut_token, expr)| ExprReference {
                mutable: mut_token.is_some(),
                expr: Box::new(expr),
                span: Span::combine(
                    Span::from_parse_span(amp),
                    expr.get_span(),
                ),
            }
        )(input)
    }
}

/**
 * Parse a return statement with optional return expression.
 */
impl Parser for ExprReturn {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "return",
            map(tuple((
                preceded(multispace0, tag("return")),
                opt(preceded(multispace1, Expr::parse)),
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
}

/**
 * Parse an unary operation.
 */
impl Parser for ExprUnary {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        let (input, _) = multispace0(input)?;
        let (span, op) = UnOp::parse(input)?;
        let (span, expr) = ExprBinary::climb(span, op.get_prec().0)?;
        Ok((span, ExprUnary {
            op: op,
            right: Box::new(expr),
            span: Span::combine(Span::from_parse_span(input), Span::from_parse_span(span)),
        }))
    }
}

/**
 * Parse a while loop.
 */
impl Parser for ExprWhile {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "while loop",
            map(tuple((
                preceded(multispace0, tag("while")),
                preceded(multispace1, Expr::parse),
                preceded(multispace0, Block::parse)
            )),
                |(start, cond, block)| {
                    ExprWhile {
                        cond: Box::new(cond),
                        block: block,
                        span: Span::combine(Span::from_parse_span(start), block.span),
                    }
                }
            )
        )(input)
    }
}

/**
 * Parser implementation for 32 bit unsigned integer literals.
 */
impl Parser for LitInt {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        let (input, digits) = preceded(multispace0, digit1)(input)?;
        match digits.fragment.parse::<i32>() {
            Ok(n) => Ok((input, LitInt{
                value: n,
                span: Span::from_parse_span(digits),
            })),
            Err(e) => Err(Err::Error(ParseError::new(digits, ErrorKind::ParseIntError(e)))),
        }
    }
}

/**
 * Parser implementation for boolean literals.
 */
impl Parser for LitBool {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        preceded(multispace0, alt((
            map(tag("true"),  |s| LitBool{value:true,  span: Span::from_parse_span(s)}),
            map(tag("false"), |s| LitBool{value:false, span: Span::from_parse_span(s)}),
        )))(input)
    }
}

/**
 * Parser implementation for string literals.
 */
impl Parser for LitStr {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        preceded(multispace0, map(tuple((
            tag("\""),
            take_while(|c: char| c != '"'),
            tag("\""),
        )), |(left, s, right): (ParseSpan, ParseSpan, ParseSpan)| {
            LitStr {
                value: s.fragment.to_string(),
                span: Span::combine(
                    Span::from_parse_span(left),
                    Span::from_parse_span(right)
                ),
            }
        }))(input)
    }
}

/**
 * Parse both multispace1 and comments.
 */
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

/**
 * Parses any type of comment and removes it from the span.
 */
fn any_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    alt((
        doc_comment,
        block_doc_comment,
        comment,
        block_comment,
    ))(input)
}

/**
 * Parse a singleline comment.
 */
fn comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    context(
        "comment",
        map(pair(tag("//"), take_until("\n")), |_| ())
    )(input)
}

/**
 * Parse a block comment.
 */
fn block_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
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

/**
 * Parse a singleline documentation comment.
 */
fn doc_comment(input: ParseSpan) -> IResult<ParseSpan, ()> {
    context(
        "doc-comment",
        map(pair(tag("///"), take_until("\n")), |_| ())
    )(input)
}

/**
 * Parse a
 */
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
    let result = String::new();
    for err in error.errors {
        let error_msg = match &err.kind {
            ErrorKind::ParseIntError(e) => &e.to_string(),
            ErrorKind::Nom(e) => (*e).description(),
            ErrorKind::Char(e) => &format!("{}", *e),
            ErrorKind::Context(e) => {
                let beg = err.span.base as usize;
                let end = err.span.base as usize + err.span.len as usize;
                &format!("expected `{}`, found `{}`", e, &source[beg..end])
            },
        };
        ErrorMsg::from_span(ErrorLevel::Error, lines, err.span, filename, source, &error_msg, "").print();
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
