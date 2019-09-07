extern crate nom;

/***************************************************************************
 * Second assignment is to parse a subset of the rust syntax and construct
 * a data structure formally an Abstract Syntax Tree (or AST). The 
 * parser should support:
 * - Functions with arguments and return values
 * - Variable declarations (incl. assignments)
 * - Binary/ Unary- Operations
 * - Branches (if statements)
 * - Loops (while loop)
 * - Types supported: i32 and bool
 *
 * Notes from previous assignment:
 * - use enum for storing different types of Operations e.g. Op::Add
 * - inherit Debug and PartialEq
 * - VerboseError stack erros give context for each parser, int, expr etc.
 * - Test the AST not the calculted value (new)
 * - Possibly use nom_locate to provide more debug information
 *     * Found at: https://github.com/fflorent/nom_locate
 ***************************************************************************/

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
    bytes::complete::tag,
    character::complete::{digit1, space0},
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
    InvalidOperator,      // The given operator is not valid.
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
            SyntaxError::InvalidOperator =>
                write!(f, "invalid operator"),
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
            SyntaxError::InvalidOperator => None,
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
 * Abstract Syntax Tree (AST)
 ***************************************************************************/


/**
 * Required Expr functions
 */
use crate::Expr::{BinOp, UnOp, Num};


// enum AbstractSynta

#[derive(Debug, Clone, PartialEq)]
enum Op {
    Equal,      // ==
    NotEq,      // !=
    LessThan,   // <
    LessEq,     // <=
    LargerThan, // >
    LargerEq,   // >=
    And,        // &&
    Or,         // ||
    Add,        // +
    Sub,        // -
    Mul,        // *
    Div,        // /
    Mod,        // %
    Not,        // !
}


impl FromStr for Op {
    type Err = SyntaxError;
    
    fn from_str(s: &str) -> Result<Op> {
        match s {
            "==" => Ok(Op::Equal),
            "!=" => Ok(Op::NotEq),
            "<"  => Ok(Op::LessThan),
            "<=" => Ok(Op::LessEq),
            ">"  => Ok(Op::LargerThan),
            ">=" => Ok(Op::LargerEq),
            "&&" => Ok(Op::And),
            "||" => Ok(Op::Or),
            "+"  => Ok(Op::Add),
            "-"  => Ok(Op::Sub),
            "*"  => Ok(Op::Mul),
            "/"  => Ok(Op::Div),
            "%"  => Ok(Op::Mod),
            "!"  => Ok(Op::Not),
            _ => Err(SyntaxError::InvalidOperator),
        }
    }
}


/**
 * Expressions can either be a binary operation (two operands),
 * unary operation (single operand)
 */
#[derive(Debug, Clone, PartialEq)]
enum Expr {
    BinOp(Box<Expr>, Op, Box<Expr>),
    UnOp(Op, Box<Expr>),
    Num(i32),
    Bool(bool),
}




/***************************************************************************
 * Addition arithmetic expression parser.
 * Note: operator precedence is considered for this assignment.
 ***************************************************************************/


/**
 * Converts string containing a number into Box::Value.
 */
fn from_number(input: &str) -> Result<Expr> {
    Num(i32::from_str(input).unwrap())
}


/**
 * Parse literal, can be either of type bool or i32.
 */
fn parse_literal(input: &str) -> IResult<&str, Expr> {
    context(
        "parse_literal",
        alt((
            map(tag("true"), |s: &str| Bool(true)),
            map(tag("false"), |s: &str| Bool(false)),
            map((digit1), |s: &str|))
        )
    )(input)        
}


fn parse_operator(input: &str) -> IResult<&str, Op> {
    context(
        "parse_operator",
        
    )
}


/**
 * Parses a simple arithmetic expression, only supports addition.
 */
fn parse_expr(input: &str) -> IResult<&str, Expr> {
    context(
        "parse_expr",
        preceded(
            multispace0,
            alt((
                //Binary operation e.g. 4 + 5
                map(tuple((parse_literal, preceded(multispace0, parse_operator), parse_expr)),
                    |(left, op, right)| Expr::BinOp(Box::new(left), op, Box::new(right))),
                
                //Unary operation e.g. -5
                map(tuple((parse_operator, parse_expr)),
                    |(op, right)| Expr::UnOp(op, Box::new(right))),

                //Literal
                parse_literal,
            ))
        )
}


/**
 * Prases the given input expression and returns a abstract syntax tree.
 */
fn parse(input: &str) -> Result<Box<Expr>> {
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
    let result = parse("   -1   +     6        / 15      ");
    match result {
        Ok(n) => println!("Ok: Calculated value: {}\n    Resulting Tree:\n    {:?}", calculate_int(&n), n),
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
fn calculate_int(value: &Box<Expr>) -> i32 {
    match &**value {
        Expr::Num(num) => return *num,
        Expr::BinOp(left, op, right) => {
            if *op == Op::Add {
                return calculate_int(&left) + calculate_int(&right);
            }
            return 0;
        }
        Expr::UnOp(op, right) => {
            if *op == Op::Sub {
                return -calculate_int(&right);
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
    assert_eq!(calculate_int(&parse("6").unwrap()),         6);
    assert_eq!(calculate_int(&parse(" 56").unwrap()),       56);
    assert_eq!(calculate_int(&parse("432    ").unwrap()),   432);
    assert_eq!(calculate_int(&parse("  4234   ").unwrap()), 4234);
}


/**
 * Testing different arithmetic expressions, only for addition.
 * Including different formatting types and also checks for
 * invalid expressions that should panic.
 */
#[test]
fn expr_test() {
    // Simple calculations
    assert_eq!(calculate_int(&parse("1+2").unwrap()),           3);
    assert_eq!(calculate_int(&parse("10+20+30").unwrap()),      60);
    assert_eq!(calculate_int(&parse("1+1+1+1+1+1+1").unwrap()), 7);
    assert_eq!(calculate_int(&parse("50000+50000").unwrap()),   100000);

    // Parseessions including weird, but correct formatting
    assert_eq!(calculate_int(&parse("1 + 2 + 3").unwrap()),           6);
    assert_eq!(calculate_int(&parse("    1    +    2    ").unwrap()), 3);
    assert_eq!(calculate_int(&parse("1 \t + \t 2").unwrap()),         3);
    assert_eq!(calculate_int(&parse("\t  1+2  \t").unwrap()),         3);

    // Invalid expressions (should return Syntax Error)
    assert!(parse("   +2   ").is_err());
    assert!(parse("  1+2+  ").is_err());
    assert!(parse(" 1 ++ 2 ").is_err());
    assert!(parse("   +    ").is_err());
}
