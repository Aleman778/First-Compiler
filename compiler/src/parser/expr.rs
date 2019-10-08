
/***************************************************************************
 * Expression parser implementation defines parsers for any
 * type of expression such as let statements, binary operations etc.
 ***************************************************************************/


use nom::{
    character::complete::{alpha1, digit1, multispace0},
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


impl Parser for Expr {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        context(
            "expression",
            alt((
                map(ExprBinOp::parse, |res| Expr::BinOp(res)),
                map(Atom::parse, |res| Expr::Atom(res)),
            ))
        )(input)
    }
}


/**
 * Parse binary operations e.g. "10 + 5", "false && true" etc.
 */
impl Parser for ExprBinOp {
    fn parse(input: ParseSpan) -> IResult<ParseSpan, Self> {
        compute_expr(input, 1)
    }
}


pub fn compute_expr(input: ParseSpan, min_prec: u8) -> IResult<ParseSpan, BinOp> {
    let (input, atom_lhs) = Atom::parse()(input)?;
    loop {
        let op_result = peek(BinOp::parse)(input);
        if op_result.is_err() {
            break;
        }
        let (input, op) = op_result?;
        let (prec, assoc) = op.get_prec();
        if prec < min_prec {
            break;
        }

        let next_prev_prec = match assoc {
            Assoc::Left => min_prec + 1,
            Assoc::
        }
        let min_prec = 

        BinOp.parse();
        let atom_rhs

        
        
    }
}

