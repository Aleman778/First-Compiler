
/***************************************************************************
 * Parses tokens that can be turned into a peekable iterator.
 ***************************************************************************/


use std::iter::Peekable;
use std::slice::Iter;


use nom::{
    character::complete::multispace0,
    sequence::preceded,
    combinator::map,
    branch::alt,
    multi::many1,
    error::context,
    Err,
};


use crate::ast::{
    expr::*,
    atom::*,
    op::*,
};


use crate::parser::{
    error::{ErrorKind, ParseError},
    ParseSpan,
    Parser,
    IResult,
};


/**
 * Tokens are terminal symbols e.g. numbers, parenthesis, operatores etc.
 */
#[derive(Debug, Clone, PartialEq)]
enum Token {
    /// The number literal token
    Num(ExprLitInt),
    /// The boolean literal token
    Bool(ExprLitBool),
    /// The parenthesis token
    Paren(ExprParen),
    /// The identifier token
    Ident(ExprIdent),
    /// The binary operator token
    BinOp(BinOp),
    /// The unary operator token
    UnOp(UnOp),
}


/**
 * Parses a single token e.g. 24, false, (2 + 5) etc.
 */
impl Parser for Token {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "token",
            preceded(multispace0, alt((
                map(ExprLitInt::parse,  |lit| Token::Num(lit)),
                map(ExprLitBool::parse, |lit| Token::Bool(lit)),
                map(ExprParen::parse, |paren| Token::Paren(paren)),
                map(ExprIdent::parse, |ident| Token::Ident(ident)),
                map(BinOp::parse,        |op| Token::BinOp(op)),
                map(UnOp::parse,         |op| Token::UnOp(op)),
            )))
        )(input)
    }
}


/**
 * Parse a mathematical expression e.g. 2 + 3 ** 2 + 4.
 */
pub fn parse_math_expr(input: ParseSpan) -> IResult<ParseSpan, Expr> {
    let (input, tokens) = many1(Token::parse)(input)?;
    let tokenizer = tokens.iter().peekable();
    let expr = climb(&mut tokenizer, 1)?;
    Ok((input, expr))
}


/**
 * Consumes the next token from the provided tokenizer and
 * computes an atom expression.
 */
fn compute_atom<'a>(tokenizer: &mut Peekable<Iter<Token>>) -> Result<Expr, Err<ParseError<'a>>> {
    match tokenizer.next() {
        Some(Token::Num(literal))  => Ok(Expr::Num(literal)),
        Some(Token::Bool(literal)) => Ok(Expr::Bool(literal)),
        Some(Token::Paren(paren))  => Ok(Expr::Paren(paren)),
        Some(Token::Ident(ident))  => Ok(Expr::Ident(ident)),
        Some(Token::UnOp(op)) => {
            let expr = climb(tokenizer, 4)?;
            Ok(Expr::UnOp(ExprUnOp{
                op: *op, right: Box::new(expr), span: get_unop_span(op),
            }))
        },
        Some(Token::BinOp(op)) => Err(Err::Error(ParseError::new(ParseSpan::new(""), ErrorKind::Context("expression")))),
        None => Err(Err::Error(ParseError::new(ParseSpan::new(""), ErrorKind::Context("expression")))),
    }
}


/**
 * Use precedence climbinig to compute a mathematically correct expression.
 */
fn climb<'a>(tokenizer: &mut Peekable<Iter<Token>>, min_prec: u8) -> Result<Expr, Err<ParseError<'a>>> {
    let expr_lhs = compute_atom(tokenizer)?;
    loop {
        let operator = match tokenizer.peek() {
            Some(Token::BinOp(op)) => op,
            _ => return Ok(expr_lhs),
        };
        
    }
}



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
