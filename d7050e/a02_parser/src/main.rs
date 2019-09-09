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
 * Required nom features for this parser.
 */
use nom::{
    bytes::complete::tag,
    character::complete::{digit1, multispace0},
    combinator::map,
    branch::alt,
    sequence::{tuple, preceded},
    error,
    Err,
};


/**
 * Require LocatedSpan from crate nom_locate.
 */
use nom_locate::LocatedSpan;


/**
 * Type alias of LocatedSpan for convenience.
 */
type Span<'a> = LocatedSpan<&'a str>;


/**
 * Error struct, defines the original span and optional local
 * span that is being parsed, with the error kind.
 */
#[derive(Debug)]
pub struct Error<'a>(Span<'a>, Option<Span<'a>>, ErrorKind);


/**
 * Type aliased IResult from std::Result.
 */
type IResult<'a, I, O, E = Error<'a>> = Result<(I, O), Err<E>>;


/**
 * Defines the different kinds of errors to expect.
 */
#[derive(Debug)]
enum ErrorKind {
    ParseIntError(std::num::ParseIntError),
    Nom(error::ErrorKind),
}


impl<'a> error::ParseError<Span<'a>> for Error<'a> {
    fn from_error_kind(input: Span<'a>, kind: error::ErrorKind) -> Self {
        Error(input, None, ErrorKind::Nom(kind))
    }

    fn append(_: Span<'a>, _: error::ErrorKind, other: Self) -> Self {
        other
    }
}


/***************************************************************************
 * Abstract Syntax Tree (AST)
 ***************************************************************************/


/**
 * The different kinds of operators used by
 * both the binary and unary operations.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
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


/**
 * Type alias operators to include span.
 */
type SpanOp<'a> = (Span<'a>, Op);


/**
 * Expressions can be a binary operation (two operands),
 * unary operation (single operand) or a literal.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    BinOp(Box<SpanExpr<'a>>, SpanOp<'a>, Box<SpanExpr<'a>>),
    UnOp(SpanOp<'a>, Box<SpanExpr<'a>>),
    Num(i32),
    Bool(bool),
}


/**
 * Type alias expressions to include span.
 */
type SpanExpr<'a> = (Span<'a>, Expr<'a>);


/***************************************************************************
 * Addition arithmetic expression parser.
 * Note: operator precedence is considered for this assignment.
 ***************************************************************************/


/**
 * Parse a binary operator.
 */
fn parse_binoperator(input: Span) -> IResult<Span, SpanOp> {
    alt((
        map(tag("=="), |s| (s, Op::Equal)),
        map(tag("!="), |s| (s, Op::NotEq)),
        map(tag("<"),  |s| (s, Op::LessThan)),
        map(tag("<="), |s| (s, Op::LessEq)),
        map(tag(">"),  |s| (s, Op::LargerThan)),
        map(tag(">="), |s| (s, Op::LargerEq)),
        map(tag("&&"), |s| (s, Op::And)),
        map(tag("||"), |s| (s, Op::Or)),
        map(tag("+"),  |s| (s, Op::Add)),
        map(tag("-"),  |s| (s, Op::Sub)),
        map(tag("*"),  |s| (s, Op::Mul)),
        map(tag("/"),  |s| (s, Op::Div)),
        map(tag("%"),  |s| (s, Op::Mod)),
    ))(input)
}


/**
 * Parse an unary operator.
 */
fn parse_unoperator(input: Span) -> IResult<Span, SpanOp> {
    alt((
        map(tag("-"),  |s| (s, Op::Sub)),
        map(tag("!"),  |s| (s, Op::Not)),
    ))(input)
}


/**
 * Parses a 32-bit signed integer.
 * Also handles errors this is not a valid i32 number.
 * Note: taken from the instructors example code.
 */
pub fn parse_i32<'a>(input: Span<'a>) -> IResult<Span<'a>, SpanExpr> {
    let (input, digits) = digit1(input)?;
    match digits.fragment.parse() {
        Ok(n) => Ok((input, (digits, Expr::Num(n)))),
        Err(e) => Err(Err::Error(Error(
            input,
            Some(digits),
            ErrorKind::ParseIntError(e),
        ))),
    }
}


/**
 * Parse literal, can be either of type bool or i32.
 * In this assignment we do not need to consider them as separate types
 * so the result can always an ambigoius expression type.
 */
pub fn parse_literal<'a>(input: Span<'a>) -> IResult<Span<'a>, SpanExpr> {
    alt((
        parse_i32,
        map(tag("true"), |s| (s, Expr::Bool(true))),
        map(tag("false"), |s| (s, Expr::Bool(false))),
    ))(input)
}


/**
 * Parses a simple arithmetic expression, only supports addition.
 */
fn parse_expr(input: Span) -> IResult<Span, SpanExpr> {
    alt((
        //Binary operation e.g. 4 + 5
        map(tuple((parse_literal, preceded(multispace0, parse_binoperator), parse_expr_ms)),
            |(left, op, right)| (input, Expr::BinOp(Box::new(left), op, Box::new(right)))),
        
        //Unary operation e.g. -5
        map(tuple((parse_unoperator, parse_expr_ms)),
            |(op, right)| (input, Expr::UnOp(op, Box::new(right)))),

        //Literal
        parse_literal,
    ))(input)
}


/**
 * Parses an expression with preceded by spaces.
 */
fn parse_expr_ms(input: Span) -> IResult<Span, SpanExpr> {
    preceded(multispace0, parse_expr)(input)
}


/***************************************************************************
 * Main method
 ***************************************************************************/


/**
 * Main method, program starts here.
 */
fn main() {
    let input = "   -1   +     6        / 15      ";
    let result = parse_expr_ms(Span::new(input));
    
    match result {
        Ok(n) => println!("Ok: Resulting Tree:\n    {:#?}", n),
        Err(e) => println!("Error: {:#?}", e),
    }
}


/***************************************************************************
 * Test cases
 ***************************************************************************/


    
/**
 * Testing simple digit values, including whitespaces.
 */
#[test]
fn test_parse_literal() {
    // 32-bit signed integer literals
    assert_eq!(parse_literal("4235"),     Ok(("", Expr::Num(4235))));
    assert_eq!(parse_literal("4235 + 2"), Ok((" + 2", Expr::Num(4235))));
    assert_eq!(parse_literal("4235abcs"), Ok(("abcs", Expr::Num(4235))));
    assert!(parse_literal("abcs4235").is_err());
    assert!(parse_literal("111111111111111111").is_err());

    // Boolean literals
    assert_eq!(parse_literal("true"),  Ok(("", Expr::Bool(true))));
    assert_eq!(parse_literal("false"), Ok(("", Expr::Bool(false))));
    assert_eq!(parse_literal("truefalse"), Ok(("false", Expr::Bool(true))));
    assert_eq!(parse_literal("falsetrue"), Ok(("true", Expr::Bool(false))));
    assert!(parse_literal("hello").is_err());
}


/**
 * Testing different arithmetic expressions, only for addition.
 * Including different formatting types and also checks for
 * invalid expressions that should panic.
 */
#[test]
fn expr_test() {
    // Simple expressions
    let expected = Expr::BinOp(Box::new(Expr::Num(1)), Op::Add, Box::new(Expr::Num(2)));
    assert_eq!(parse_expr("1+2"), Ok(("", expected)));
    assert_eq!(parse_expr("1+2+1"), Ok(("+1", expected)));
}
