extern crate nom;

/**
 * Required nom features for this parser.
 */
use nom::{
    IResult,
    character::complete::{char, digit1},
    sequence::tuple,
    combinator::opt
};

/**
 * Required BTree functions
 */
use crate::BTree::{Op, Val, Nil};


/**
 * Binary tree, branches are operators and leaves are operands.
 */
#[derive(Debug)]
enum BTree {
    Op(char, Box<BTree>, Box<BTree>),
    Val(i32),
    Nil,
}


/**
 * Converts string to 32 bit integer.
 */
fn to_i32(input: &str) -> i32 {
    return input.parse::<i32>().unwrap();
}


/**
 * Addition parser is used to parse a+b or +b with left binary tree.
 */
fn addition_parser(input: &str, left: Option<Box<BTree>>) -> IResult<&str, Box<BTree>> {
    let (input, res) = tuple((opt(digit1), char('+'), digit1))(input)?;
    let mut operand1 = Box::new(Nil);
    if res.0.is_some() {
        operand1 = Box::new(Val(to_i32(res.0.unwrap())));
    } else if left.is_some() {
        operand1 = left.unwrap();
    }
    let operand2 = Box::new(Val(to_i32(res.2)));
    let operator = Box::new(Op('+', operand1, operand2));
    return Ok((input, operator));
}


/**
 * Expression parser can parse simple arithmetic expressions,
 * currently only supports addition. Parses e.g. 1+2+3 to the
 * following AST: Op('+', Op('+', Val(1), Val(2)), Val(3)).
 */
fn expr_parser(input: &str) -> IResult<&str, Box<BTree>> {
    let mut input = input;
    let mut tree: Option<Box<BTree>> = None;
    while !input.is_empty() {
        let (inpt, res) = addition_parser(input, tree).unwrap();
        input = inpt;
        tree = Some(res);
    }
    return Ok((input, tree.unwrap()));
}


/**
 * Main method, program starts here.
 */
fn main() {
    let tree = expr_parser("1+2+3");
    println!("tree = {:?}", tree);
}
