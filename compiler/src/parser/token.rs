
/***************************************************************************
 * Parses tokens that can be turned into a peekable iterator.
 ***************************************************************************/


use std::iter::Peekable;
use std::slice::Iter;


use nom::{
    character::complete::{alpha1, digit1, multispace0},
    bytes::complete::{tag, take_while1},
    character::{is_alphanumeric},
    combinator::{map, peek},
    sequence::{pair, preceded, delimited},
    branch::alt,
    multi::many1,
    error::context,
    Err,
};


use crate::ast::{
    span::Span,
    expr::*,
    atom::*,
    op::*,
};


use crate::parser::{
    error::{ErrorKind, ParseError},
    ParseSpan,
    IResult,
};


/**
 * Tokens are terminal symbols e.g. numbers, parenthesis, operatores etc.
 */
#[derive(Debug, Clone, PartialEq)]
enum Token<'a> {
    /// The number literal token
    Num(i32),
    /// The boolean literal token
    Bool(bool),
    /// The parenthesis token
    Paren(Vec<SpanToken<'a>>),
    /// The identifier token
    Ident(&'a str),
    /// The binary operator token
    Op(Op),
}


/**
 * Type alias of token to include span information.
 */
type SpanToken<'a> = (ParseSpan<'a>, Token<'a>);

type Tokenizer<'a> = Peekable<Iter<'a, SpanToken<'a>>>;


/**
 * Parse binary operators
 */
fn parse_op(input: ParseSpan) -> IResult<ParseSpan, (ParseSpan, Op)> {
    alt((
        map(tag("+"),  |s| (s, Op::Add{span: Span::new(s)})),
        map(tag("-"),  |s| (s, Op::Sub{span: Span::new(s)})),
        map(tag("**"), |s| (s, Op::Pow{span: Span::new(s)})),
        map(tag("*"),  |s| (s, Op::Mul{span: Span::new(s)})),
        map(tag("/"),  |s| (s, Op::Div{span: Span::new(s)})),
        map(tag("%"),  |s| (s, Op::Mod{span: Span::new(s)})),
        map(tag("&&"), |s| (s, Op::And{span: Span::new(s)})),
        map(tag("||"), |s| (s, Op::Or{span: Span::new(s)})),
        map(tag("=="), |s| (s, Op::Eq{span: Span::new(s)})),
        map(tag("!="), |s| (s, Op::Ne{span: Span::new(s)})),
        map(tag("<"),  |s| (s, Op::Lt{span: Span::new(s)})),
        map(tag("<="), |s| (s, Op::Le{span: Span::new(s)})),
        map(tag(">"),  |s| (s, Op::Gt{span: Span::new(s)})),
        map(tag(">="), |s| (s, Op::Ge{span: Span::new(s)})),
        map(tag("!"),  |s| (s, Op::Not{span: Span::new(s)})),
    ))(input)
}


/**
 * Parser implementation for 32 bit unsigned integer literals.
 */
fn parse_i32(input: ParseSpan) -> IResult<ParseSpan, (ParseSpan, i32)> {
    let (input, digits) = digit1(input)?;
    match digits.fragment.parse::<i32>() {
        Ok(n) => Ok((input, (digits, n))),
        Err(e) => Err(Err::Error(ParseError::new(digits, ErrorKind::ParseIntError(e)))),
    }
}


/**
 * Parser implementation for boolean literals.
 */
fn parse_bool(input: ParseSpan) -> IResult<ParseSpan, (ParseSpan, bool)> {
    alt((
        map(tag("true"),  |s| (s, true)),
        map(tag("false"), |s| (s, false)),
    ))(input)
}


/**
 * Parse an identifier e.g. test_id.
 */
pub fn parse_ident<'a>(input: ParseSpan<'a>) -> IResult<ParseSpan<'a>, (ParseSpan<'a>, &'a str)> {
    map(pair(
        peek(alt((alpha1, tag("_")))),
        take_while1(|c: char| is_alphanumeric(c as u8) || c == '_')
    ), |(_, s): (ParseSpan<'a>, ParseSpan<'a>)| (s, s.fragment)
    )(input)   
}


/**
 * Parse a parenthesized expression e.g. (2 + 6) 
 */
fn parse_paren<'a>(input: ParseSpan<'a>) -> IResult<ParseSpan<'a>, (ParseSpan<'a>, Vec<SpanToken<'a>>)> {
    map(delimited(tag("("), many1(parse_token), preceded(multispace0, tag(")"))),
        |tokens| (input, tokens)
    )(input)
}


/**
 * Parses a single token e.g. 24, false, (2 + 5) etc.
 */
fn parse_token(input: ParseSpan) -> IResult<ParseSpan, SpanToken> {
    context(
        "expression or operator",
        preceded(multispace0, alt((
            map(parse_op, |(s, op)| (s, Token::Op(op))),
            map(parse_i32, |(s, val)| (s, Token::Num(val))),
            map(parse_bool, |(s, val)| (s, Token::Bool(val))),
            map(parse_ident, |(s, id)| (s, Token::Ident(id))),
            map(parse_paren, |(s, tokens)| (s, Token::Paren(tokens))),
        )))
    )(input)
}


/**
 * Parse a mathematical expression e.g. 2 + 3 ** 2 + 4.
 */
pub fn parse_math_expr(input: ParseSpan) -> IResult<ParseSpan, Expr> {
    let (input, tokens) = many1(parse_token)(input)?;
    let mut tokenizer = tokens.iter().peekable();
    match climb(&mut tokenizer, 1) {
        Ok((_, expr)) => Ok((input, expr)),
        Err(_) => Err(Err::Error(ParseError::new(input, ErrorKind::Context("math expression")))),
    }
}


/**
 * Consumes the next token from the provided tokenizer and
 * computes an atom expression.
 */
fn compute_atom<'a>(tokenizer: &mut Tokenizer<'a>) -> IResult<'a, ParseSpan<'a>, Expr> {
    match tokenizer.next() {
        Some((span, token)) => match token {
            Token::Num(val) => Ok((*span, Expr::Num(LitInt{value: *val, span: Span::new(*span)}))),
            Token::Bool(val) => Ok((*span, Expr::Bool(LitBool{value: *val, span: Span::new(*span)}))),
            Token::Paren(tokens)  => {
                let (span, expr) = climb(&mut tokens.iter().peekable(), 1)?;
                Ok((span, Expr::Paren(ExprParen{expr: Box::new(expr), span: Span::new(span)})))
            },
            Token::Ident(id)  => Ok((*span, Expr::Ident(ExprIdent{to_string: id.to_string(), span: Span::new(*span)}))),
            Token::Op(op) => {
                let (span, expr) = climb(tokenizer, 4)?;
                Ok((span, Expr::UnOp(ExprUnOp{op: *op, right: Box::new(expr), span: Span::new(span)})))
            },
        },
        None => Err(Err::Error(ParseError::new(ParseSpan::new(""), ErrorKind::Context("expression")))),
    }
}


/**
 * Use precedence climbinig to compute a mathematically correct expression.
 */
fn climb<'a>(tokenizer: &mut Tokenizer<'a>, min_prec: u8) -> IResult<'a, ParseSpan<'a>, Expr> {
    let (span_lhs, mut expr_lhs) = compute_atom(tokenizer)?;
    loop {
        match tokenizer.peek() {
            Some((span, Token::Op(op))) => {
                let (prec, assoc) = get_prec(&op);
                if prec < min_prec {
                    break;
                }
                let next_min_prec = match assoc {
                    Assoc::Left => prec + 1,
                    Assoc::Right => prec,
                };
                tokenizer.next();
                let (_span_rhs, expr_rhs) = climb(tokenizer, next_min_prec)?;
                expr_lhs = Expr::BinOp(ExprBinOp{
                    left: Box::new(expr_lhs),
                    op: *op,
                    right: Box::new(expr_rhs),
                    span: Span::new(*span),
                });
            },
            _ => break,
        }
    }
    Ok((span_lhs, expr_lhs))
}

/*
/**
 * Constructs a new parse span by combining multiple parse spans.
 * Note: the input span array has to be in correct order.
 */
pub fn combine_span<'a>(new_span: &'a mut ParseSpan<'a>, spans: &[&ParseSpan]) {
    let mut fragment = String::new();
    for span in spans {
        fragment.push_str(span.fragment);
    }
    new_span.fragment = fragment.as_str();
    new_span.line = spans[0].line;
    new_span.offset = spans[0].offset;
    new_span.extra = spans[0].extra;
}
*/

/*

pub fn parse_binop(input: ParseSpan) -> IResult<ParseSpan, Expr> {
    compute_expr(input, 1)
}


/**
 * Compute the expression using precedence climbining algorithm.
 */
pub fn compute_expr(mut input: ParseSpan, min_prec: u8) -> IResult<ParseSpan, Expr> {
    let (inpt, atom_lhs) = Atom::parse(input)?;
    input = inpt;
    let mut expr_lhs = Expr::Atom(atom_lhs);
    loop {
        let op_result = peek(BinOp::parse)(input);
        if op_result.is_err() {
            break;
        }
        let (inpt, op) = op_result?;
        let (prec, assoc) = get_prec(&op);
        if prec < min_prec {
            break;
        }x

        let next_prev_prec = match assoc {
            Assoc::Left => prec + 1,
            Assoc::Right => prec,
        };

        let (inpt, op) = BinOp::parse(input)?;
        input = inpt;
        let (inpt, expr_rhs) = compute_expr(input, next_prev_prec)?;
        input = inpt;
        
        let binop = ExprBinOp{
            left: Box::new(expr_lhs),
            op: op,
            right: Box::new(expr_rhs),
            span: Span::new(input),
        };
        expr_lhs = Expr::BinOp(binop);
    }

    Ok((input, expr_lhs))
}
*/
