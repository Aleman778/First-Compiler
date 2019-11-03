
/***************************************************************************
 * Expression parser implementation defines parsers for any
 * type of expression such as let statements, binary operations etc.
 ***************************************************************************/


use nom::{
    character::is_alphanumeric,
    character::complete::{alpha1, multispace0, multispace1},
    bytes::complete::{take_while1, tag},
    combinator::{map, opt, peek},
    sequence::{preceded, pair, tuple},
    multi::{many0, separated_list},
    branch::alt,
    error::context,
};


use crate::ast::{
    span::{LineColumn, Span},
    ty::Type,
    expr::*,
    lit::*,
    op::*,
};


use crate::parser::{
    ParseSpan,
    IResult,
    Parser,
};


/**
 * Parse an expression, for mathematical expressions use parse_math instead.
 */
impl Parser for Expr {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "expression",
            alt((
                map(ExprLocal::parse,    |expr_local|    Expr::Local(expr_local)),
                map(ExprAssign::parse,   |expr_assign|   Expr::Assign(expr_assign)),
                map(ExprIf::parse,       |expr_if|       Expr::If(expr_if)),
                map(ExprWhile::parse,    |expr_while|    Expr::While(expr_while)),
                map(ExprBlock::parse,    |expr_block|    Expr::Block(expr_block)),
                map(ExprReturn::parse,   |expr_return|   Expr::Return(expr_return)),
                map(ExprBreak::parse,    |expr_break|    Expr::Break(expr_break)),
                map(ExprContinue::parse, |expr_continue| Expr::Continue(expr_continue)),
                map(ExprCall::parse,     |expr_call|     Expr::Call(expr_call)),
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
        context(
            "expression",
            preceded(multispace0, ExprBinary::parse)
        )(input)
    }

    
    /**
     * Parse an atom i.e. literal, parenthesized expression, function call,
     * identifier or unary operation.
     */
    pub fn parse_atom(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "expression",
            alt((
                map(ExprLit::parse,       |literal| Expr::Lit(literal)),
                map(ExprParen::parse,     |expr|    Expr::Paren(expr)),
                map(ExprCall::parse_term, |call|    Expr::Call(call)),
                map(ExprIdent::parse,     |ident|   Expr::Ident(ident)),
                map(ExprUnary::parse,     |unary|   Expr::Unary(unary)),
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
                    ExprIdent::parse,
                    preceded(multispace0, tag("=")),
                    Expr::parse_math,
                    preceded(multispace0, tag(";")),
                )),
                    |(id, _, expr, end)| {
                        let rid = id.clone();
                        ExprAssign {
                            ident: id,
                            expr: Box::new(expr),
                            span: Span::from_bounds(
                                rid.span.start,
                                LineColumn::new(end.line, end.get_column() + 1),
                                input.extra,
                            ),
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
                        span: Span::from_bounds(
                            LineColumn::new(input.line, input.get_column()),
                            LineColumn::new(output.line, output.get_column()),
                            input.extra,
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
 * Parse expression block.
 */
impl Parser for ExprBlock {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "block statement",
            map(tuple((
                preceded(multispace0, tag("{")),
                many0(Expr::parse),
                opt(Expr::parse_math),
                preceded(multispace0, tag("}")),
            )),
                |(start, mut stmts, ret, end)| {
                    if ret.is_some() {
                        stmts.push(ret.unwrap());
                    }
                    ExprBlock{
                        stmts: stmts,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            LineColumn::new(end.line, end.get_column() + 1),
                            input.extra,
                        ),
                    }
                }
            )
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
                preceded(multispace0, tag(";"))
            ),
                |(start, end) : (ParseSpan, ParseSpan)| ExprBreak {
                    span: Span::from_bounds(
                        LineColumn::new(start.line, start.get_column()),
                        LineColumn::new(end.line, end.get_column() + 1),
                        input.extra,
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
            preceded(multispace0, tag(";"))
        ),
            |(mut call, end)| {
                call.span.end = LineColumn::new(end.line, end.get_column() + 1);
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
                separated_list(preceded(multispace0, tag(",")), Expr::parse_math),
                preceded(multispace0, tag(")")),
            )),
                |(id, _, args, end)| {
                    let rid = id.clone();
                    ExprCall {
                        ident: id,
                        args: args,
                        span: Span::from_bounds(
                            rid.span.start,
                            LineColumn::new(end.line, end.get_column() + 1),
                            input.extra,
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
                preceded(multispace0, tag(";"))
            ),
                |(start, end) : (ParseSpan, ParseSpan)| ExprContinue {
                    span: Span::from_bounds(
                        LineColumn::new(start.line, start.get_column()),
                        LineColumn::new(end.line, end.get_column() + 1),
                            input.extra,
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
                    span: Span::new(s)
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
                preceded(multispace1, Expr::parse_math),
                preceded(multispace0, ExprBlock::parse),
                opt(preceded(
                    pair(multispace0, tag("else")),
                    preceded(multispace0, ExprBlock::parse))
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
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            end.span.end,
                            input.extra,
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
 * Parse a local variable (let binding).
 */
impl Parser for ExprLocal {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "local variable",
            map(tuple((
                preceded(multispace0, tag("let")),
                opt(preceded(multispace1, tag("mut"))),
                preceded(multispace1, ExprIdent::parse),
                preceded(multispace0, tag(":")),
                preceded(multispace0, Type::parse),
                preceded(multispace0, tag("=")),
                preceded(multispace0, Expr::parse_math),
                preceded(multispace0, tag(";")),
            )),
                |(start, mutable, ident, _, ty, _, expr, end)| ExprLocal {
                    mutable: mutable.is_some(),
                    ident: ident,
                    ty: ty,
                    init: Box::new(expr),
                    span: Span::from_bounds(
                        LineColumn::new(start.line, start.get_column()),
                        LineColumn::new(end.line, end.get_column() + 1),
                        input.extra,
                    ),
                }
            )
        )(input)
    }
}


/**
 * Parse a prenthesized expression.
 */
impl Parser for ExprParen {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        map(tuple((
            preceded(multispace0, tag("(")),
            Expr::parse_math,
            preceded(multispace0, tag(")"))
        )),
            |(start, expr, end)| ExprParen {
                expr: Box::new(expr),
                span: Span::from_bounds(
                    LineColumn::new(start.line, start.get_column()),
                    LineColumn::new(end.line, end.get_column() + 1),
                    input.extra,
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
                opt(preceded(multispace1, Expr::parse_math)),
                preceded(multispace0, tag(";")),
            )),
                |(start, expr, end)| ExprReturn {
                    expr: Box::new(expr),
                    span: Span::from_bounds(
                        LineColumn::new(start.line, start.get_column()),
                        LineColumn::new(end.line, end.get_column() + 1),
                        input.extra,
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
            span: Span::from_bounds(
                LineColumn::new(input.line, input.get_column()),
                LineColumn::new(span.line, span.get_column()),
                input.extra,
            ),
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
                preceded(multispace1, Expr::parse_math),
                preceded(multispace0, ExprBlock::parse)
            )),
                |(start, cond, block)| {
                    let rblock = block.clone();
                    ExprWhile {
                        cond: Box::new(cond),
                        block: block,
                        span: Span::from_bounds(
                            LineColumn::new(start.line, start.get_column()),
                            rblock.span.end,
                            input.extra,
                        ),
                    }
                }
            )
        )(input)
    }
}
