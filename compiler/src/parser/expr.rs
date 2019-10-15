
/***************************************************************************
 * Expression parser implementation defines parsers for any
 * type of expression such as let statements, binary operations etc.
 ***************************************************************************/


use nom::{
    character::is_alphanumeric,
    character::complete::{alpha1, multispace0, multispace1},
    bytes::complete::{take_while1, tag},
    combinator::{map, opt, peek},
    sequence::{delimited, preceded, terminated, pair, tuple},
    multi::{many0, separated_list},
    branch::alt,
    error::context,
};


use crate::ast::{
    span::Span,
    base::Type,
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
                Expr::parse_atom,
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
            alt((
                ExprBinary::parse,
                Expr::parse_atom,
            ))
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
                map(ExprLit::parse, |literal| Expr::Lit(literal)),
                map(ExprParen::parse, |expr| Expr::Paren(expr)),
                map(ExprCall::parse, |call| Expr::Call(call)),
                map(ExprIdent::parse, |ident| Expr::Ident(ident)),
                map(ExprUnary::parse, |unary| Expr::Unary(unary)),
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
                    |(id, _, expr, _)| ExprAssign {
                        ident: id,
                        expr: Box::new(expr),
                        span: Span::new(input),
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
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Expr> {
        ExprBinary::climb(input, 1)
    }

    
    /**
     * Parse a binary operation using precedence climbing algorithm.
     */
    fn climb(input: ParseSpan, min_prec: u8) -> IResult<ParseSpan, Expr> {
        let (mut input, mut expr_lhs) = Expr::parse_atom(input)?;
        loop {
            match peek(BinOp::parse)(input) {
                Ok((_span, operator)) => {
                    let (prec, assoc) = operator.get_prec();
                    if prec < min_prec {
                        break;
                    }
                    let next_min_prec = match assoc {
                        Assoc::Left => prec + 1,
                        Assoc::Right => prec,
                    };
                    let (span, operator) = BinOp::parse(input)?;
                    let (span, expr_rhs) = ExprBinary::climb(span, next_min_prec)?;
                    expr_lhs = Expr::Binary(ExprBinary {
                        left: Box::new(expr_lhs),
                        op: operator,
                        right: Box::new(expr_rhs),
                        span: Span::new(span),
                    });
                    input = span;
                },
                _ => break,
            }
        }
        Ok((input, expr_lhs))
    }
}


/**
 * Parse expression block.
 */
impl Parser for ExprBlock {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "block statement",
            map(pair(
                delimited(
                    preceded(multispace0, tag("{")),
                    many0(Expr::parse),
                    preceded(multispace0, tag("}"))
                ),
                opt(Expr::parse_math)
            ),
                |(mut stmts, ret)| {
                    if ret.is_some() {
                        stmts.push(ret.unwrap());
                    }
                    
                    ExprBlock{
                        stmts: stmts,
                        span: Span::new(input),
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
            map(preceded(
                multispace0,
                terminated(
                    tag("break"),
                    preceded(multispace0, tag(";"))
                )
            ),
                |_| ExprBreak{span: Span::new(input)}
            )
        )(input)
    }
}


/**
 * Parse function call.
 */
impl Parser for ExprCall {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "function call",
            map(tuple((
                ExprIdent::parse,
                preceded(multispace0, tag("(")),
                separated_list(preceded(multispace0, tag(",")), Expr::parse_math),
                preceded(multispace0, tag(")")),
                preceded(multispace0, tag(";")),
            )),
                |(id, _, args, _, _)| ExprCall {
                    ident: id,
                    args: args,
                    span: Span::new(input),
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
            map(preceded(
                multispace0,
                terminated(
                    tag("continue"),
                    preceded(multispace0, tag(";"))
                )
            ),
                |_| ExprContinue{span: Span::new(input)}
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
            map(pair(
                peek(alt((alpha1, tag("_")))),
                take_while1(|c: char| is_alphanumeric(c as u8) || c == '_')),
                |(_, s): (ParseSpan, ParseSpan)| ExprIdent {
                    to_string: s.fragment.to_string(),
                    span: Span::new(s)
                }
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
            preceded(
                tag("if"),
                map(tuple((
                    preceded(multispace1, Expr::parse_math),
                    preceded(multispace0, ExprBlock::parse),
                    opt(preceded(
                        pair(multispace0, tag("else")),
                        preceded(multispace0, ExprBlock::parse))
                    ),
                )),
                    |(cond, then_block, else_block)| ExprIf {
                        cond: Box::new(cond),
                        then_block: then_block,
                        else_block: else_block,
                        span: Span::new(input),
                    }
                )
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
                map(LitInt::parse, |literal| ExprLit{lit: Lit::Int(literal), span: Span::new(input)}),
                map(LitBool::parse, |literal| ExprLit{lit: Lit::Bool(literal), span: Span::new(input)}),
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
            preceded(
                tag("let"),
                map(tuple((
                    opt(preceded(multispace1, tag("mut"))),
                    preceded(multispace1, ExprIdent::parse),
                    preceded(multispace0, tag(":")),
                    preceded(multispace0, Type::parse),
                    preceded(multispace0, tag("=")),
                    preceded(multispace0, Expr::parse_math),
                    preceded(multispace0, tag(";")),
                )),
                    |(mutable, ident, _, ty, _, expr, _)| ExprLocal {
                        mutable: mutable.is_some(),
                        ident: ident,
                        ty: ty,
                        init: Box::new(expr),
                        span: Span::new(input),
                    }
                )
            )
        )(input)
    }
}


/**
 * Parse a prenthesized expression.
 */
impl Parser for ExprParen {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        map(delimited(tag("("), Expr::parse_math, preceded(multispace0,tag(")"))),
            |expr| ExprParen{expr: Box::new(expr), span: Span::new(input)}
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
                |(_, expr, _)| ExprReturn {
                    expr: Box::new(expr),
                    span: Span::new(input),
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
        context(
            "unary operation",
            map(pair(
                UnOp::parse,
                ExprUnary::parse_unop
            ),
                |(op, expr)| ExprUnary {
                    op: op,
                    right: Box::new(expr),
                    span: Span::new(input),
                }
            )
        )(input)
    }
}


/**
 * Implementation of unary parser.
 */
impl ExprUnary {
    /**
     * Precedence climbing for unary operator, assume highest precedence.
     */
    fn parse_unop(input: ParseSpan) -> IResult<ParseSpan, Expr> {
        ExprBinary::climb(input, 7)
    }
}


/**
 * Parse a while loop.
 */
impl Parser for ExprWhile {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "while loop",
            preceded(
                tag("while"),
                map(pair(
                    preceded(multispace1, Expr::parse_math),
                    preceded(multispace0, ExprBlock::parse)
                ),
                    |(cond, block)| ExprWhile {
                        cond: Box::new(cond),
                        block: block,
                        span: Span::new(input),
                    }
                )
            )
        )(input)
    }
}
