extern crate nom;


/**
 * Require from standard library.
 */
use std::{
    error,
    fmt,
    num::ParseIntError,
    str::FromStr
};


/**
 * Required nom features for this parser.
 */
use nom::{
    IResult,
    character::complete::{char, digit1, space0},
    combinator::{map_res},
    multi::fold_many0,
    sequence::{pair, delimited}
};


/***************************************************************************
 * Setup syntax error
 ***************************************************************************/


/**
 * Type aliases Result to include syntax error by default.
 */
type Result<T> = std::result::Result<T, SyntaxError>;


/**
 * Syntax error struct, contains different type of suberrors.
 * e.g. invalid expression or int parser error etc.
 */
#[derive(Debug, Clone)]
enum SyntaxError {
    InvalidExpression,    // When the entire input wasn't parsed.
    Parse(ParseIntError), // When string failed to be parsed into int.
}


/**
 * Format message generated for syntax error.
 */
impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SyntaxError::InvalidExpression =>
                write!(f, "invalid expression"),
            SyntaxError::Parse(ref e) => e.fmt(f),
        }
    }
}


/**
 * Implement syntax error as an actual error.
 */
impl error::Error for SyntaxError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            SyntaxError::InvalidExpression => None,
            SyntaxError::Parse(ref e) => Some(e),
        }
    }
}


impl From<ParseIntError> for SyntaxError {
    fn from(err: ParseIntError) -> SyntaxError {
        SyntaxError::Parse(err)
    }
}


/***************************************************************************
 * Binary tree data structure
 ***************************************************************************/


/**
 * Required BTree functions
 */
use crate::BTree::{Op, Val};


/**
 * Binary tree, branches are operators and leaves are operands.
 */
#[derive(Debug, Clone)]
enum BTree {
    Op(char, Box<BTree>, Box<BTree>),
    Val(i32)
}


/***************************************************************************
 * Addition arithmetic expression parser.
 ***************************************************************************/


/**
 * Converts string containing a number into Box::Value.
 */
fn from_number(input: &str) -> Result<Box<BTree>> {
    Ok(Box::new(Val(i32::from_str(input.trim()).unwrap())))
}


/**
 * Parses a simple number including blankspaces.
 * e.g. matches "   x   " and creates a box BTree::Val(x).
 */
fn number(input: &str) -> IResult<&str, Box<BTree>> {
    map_res(                               // Maps number to the resulting BTree::Val.
        delimited(space0, digit1, space0), // Matches numbers between possible whitespace characters
        from_number                        // Converts str to BTree::Val.
    )(input)
}


/**
 * Parses a simple arithmetic expression, only supports addition.
 */
fn expr(input: &str) -> IResult<&str, Box<BTree>> {
    let (input, left) = number(input)?;
    
    fold_many0(
        pair(char('+'), number),
        left,
        |mut left: Box<BTree>, (op, right): (char, Box<BTree>)| {
            left = Box::new(Op(op, left, right));
            left
        }
    )(input)
}


/**
 * Prases the given input expression and returns a abstract syntax tree.
 */
fn parse(input: &str) -> Result<Box<BTree>> {
    let result = expr(input);
    if result.is_err() {
        return Err(SyntaxError::InvalidExpression);
    }
    
    let (input, tree) = result.unwrap();
    if !input.is_empty() {
        return Err(SyntaxError::InvalidExpression);
    }
    
    return Ok(tree);
}


/***************************************************************************
 * Main method
 ***************************************************************************/


/**
 * Main method, program starts here.
 */
fn main() {
    let result = parse("5 + 7 + 13");
    match result {
        Ok(n) => println!("Ok: Calculated value: {}\n    Resulting Tree:\n    {:?}", calculate(&n), n),
        Err(e) => println!("Error: {}", e),
    }
}


/***************************************************************************
 * Test cases
 ***************************************************************************/


/**
 * Recursively calculates the resulting value of the binary tree.
 * Currently only supports addition.
 */
fn calculate(value: &Box<BTree>) -> i32 {
    match &**value {
        BTree::Val(val) => return *val,
        BTree::Op(op, left, right) => {
            if *op == '+' {
                return calculate(&left) + calculate(&right);
            }
            return 0;
        }
    }
}

    
/**
 * Testing simple digit values, including whitespaces.
 */
#[test]
fn number_test() {
    assert_eq!(calculate(&parse("6").unwrap()),         6);
    assert_eq!(calculate(&parse(" 56").unwrap()),       56);
    assert_eq!(calculate(&parse("432    ").unwrap()),   432);
    assert_eq!(calculate(&parse("  4234   ").unwrap()), 4234);
}


/**
 * Testing different arithmetic expressions, only for addition.
 * Including different formatting types and also checks for
 * invalid expressions that should panic.
 */
#[test]
fn expr_test() {
    // Simple calculations
    assert_eq!(calculate(&parse("1+2").unwrap()),           3);
    assert_eq!(calculate(&parse("10+20+30").unwrap()),      60);
    assert_eq!(calculate(&parse("1+1+1+1+1+1+1").unwrap()), 7);
    assert_eq!(calculate(&parse("50000+50000").unwrap()),   100000);

    // Parseessions including weird, but correct formatting
    assert_eq!(calculate(&parse("1 + 2 + 3").unwrap()),           6);
    assert_eq!(calculate(&parse("    1    +    2    ").unwrap()), 3);
    assert_eq!(calculate(&parse("1 \t + \t 2").unwrap()),         3);
    assert_eq!(calculate(&parse("\t  1+2  \t").unwrap()),         3);

    // Invalid expressions (should return Syntax Error)
    assert!(parse("   +2   ").is_err());
    assert!(parse("  1+2+  ").is_err());
    assert!(parse(" 1 ++ 2 ").is_err());
    assert!(parse("   +    ").is_err());
}
