
/***************************************************************************
 * Expression parser implementation defines parsers for any
 * type of expression such as let statements, binary operations etc.
 ***************************************************************************/


use nom::{
    character::complete::multispace0,
    character::{is_alphanumeric},
    bytes::complete::{tag, take_while1},
    combinator::{map, peek},
    sequence::{preceded, pair, tuple},
    branch::alt,
    error::context,
};


use crate::ast::{
    span::Span,
    atom::Atom,
    expr::*,
    op::*,
};


use crate::parser::{
    error::{ParseError, ErrorKind},
    ParseSpan,
    IResult,
    Parser,
};


/* TEMPLATE USED FOR COPYING

impl Parser for  {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "",
        )(input)
    }
}

*/


/**
 * Parse an expression.
 */
impl Parser for Expr {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "expression",
            alt((
                map(parse_binop, |expr| expr),
                map(Atom::parse, |res| Expr::Atom(res)),
            ))
        )(input)
    }
}


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
        }

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

