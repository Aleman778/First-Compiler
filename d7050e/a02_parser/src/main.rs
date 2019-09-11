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
    bytes::complete::{tag, take_while},
    character::complete::{digit1, alpha1, multispace0, multispace1},
    character::{is_alphanumeric},
    combinator::{map, peek, opt},
    branch::alt,
    multi::fold_many0,
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
 * Expressions can be a binary operation (two operands),
 * unary operation (single operand) or a literal.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    // Binary Operator
    BinOp(Box<SpanExpr<'a>>, SpanOp<'a>, Box<SpanExpr<'a>>),

    // Unary operator
    UnOp(SpanOp<'a>, Box<SpanExpr<'a>>),

    // Local variable declaration
    Local(Box<SpanExpr<'a>>, SpanType<'a>, Box<SpanExpr<'a>>),

    // Assignment of mutable local variable
    Assign(Box<SpanExpr<'a>>, Box<SpanExpr<'a>>),

    // Function call
    Call(Box<SpanExpr<'a>>, Vec<SpanExpr<'a>>),

    // Return expression
    Return(Box<SpanExpr<'a>>),

    // Identifier
    Ident(&'a str),

    // Number literal
    Num(i32),

    // Boolean literal
    Bool(bool),
}


/**
 * Type alias of expressions to include span.
 */
type SpanExpr<'a> = (Span<'a>, Expr<'a>);


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
 * Type alias of operator to include span.
 */
type SpanOp<'a> = (Span<'a>, Op);


/**
 * The different kinds of supported types.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int32,
    Bool,
}


/**
 * Type alias of type enum to include span.
 */
type SpanType<'a> = (Span<'a>, Type);


/**
 * Define block as a vector statements.
 */
#[derive(Debug)]
pub struct Block<'a>(Vec<Expr<'a>>);



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
        // Binary operation e.g. 4 + 5
        map(tuple((parse_literal, preceded(multispace0, parse_binoperator), parse_expr_ms)),
            |(left, op, right)| (input, Expr::BinOp(Box::new(left), op, Box::new(right)))),
        
        // Unary operation e.g. -5
        map(tuple((parse_unoperator, parse_expr_ms)),
            |(op, right)| (input, Expr::UnOp(op, Box::new(right)))),

        // Local variable declaration e.g. let a: i32 = 5
        parse_local,
        
        // Literal
        parse_literal,
    ))(input)
}


/**
 * Parses an expression with preceded by spaces.
 */
fn parse_expr_ms(input: Span) -> IResult<Span, SpanExpr> {
    preceded(multispace0, parse_expr)(input)
}


/**
 * Parse a type, e.g. i32 or bool.
 */
fn parse_type(input: Span) -> IResult<Span, SpanType> {
    alt((
        map(tag("i32"),  |s| (s, Type::Int32)),
        map(tag("bool"),  |s| (s, Type::Bool)),
    ))(input)
}


/**
 * Parses an identifier, has to start with an alphabetic character.
 * e.g. to_string, testVar, hello30.
 */
fn parse_identifier(input: Span) -> IResult<Span, SpanExpr> {
    peek(alpha1)(input)?;
    map(take_while(|c: char| is_alphanumeric(c as u8)),
        |s: Span| (s, Expr::Ident(s.fragment))
    )(input)
}


/**
 * Parse a function call. e.g. min(a + 5, 30).
 */
fn parse_fn_call(input: Span) -> IResult<Span, SpanExpr> {
    map(tuple((
        parse_identifier,
        preceded(multispace0, tag("(")),
        opt(preceded(multispace0, parse_expr)),
        fold_many0(
            preceded(delimited(multispace0, tag(","), multispace0), parse_expr)
        ),
        preceded(multispace0, tag(")")),
        preceded(multispace0, tag("{")),
        parse_block,
        preceded(multispace0, tag("}"))
    )),
        |(ident, _, arg0, arg_rest, )|
}


/**
 * Parse assignment of mutable local variables
 */
fn parse_assign(input: Span) -> IResult<Span, SpanExpr> {
    map(tuple((
        parse_identifier,
        preceded(multispace0, tag("=")),
        preceded(multispace0, parse_expr),
        preceded(multispace0, tag(";")),
    )),
        |(ident, _, expr, _)| (input, Expr::Assign(Box::new(ident), Box::new(expr)))
    )(input)
}


/**
 * Parse declarations of local variables
 * e.g. let a: i32 = 5 + 7;
 */
fn parse_local(input: Span) -> IResult<Span, SpanExpr> {
    preceded(
        tag("let"),
        map(tuple((
            preceded(multispace1, parse_identifier),
            preceded(multispace0, tag(":")),
            preceded(multispace0, parse_type),
            preceded(multispace0, tag("=")),
            preceded(multispace0, parse_expr),
            preceded(multispace0, tag(";")),
        )),
            |(ident, _, ty, _, expr, _)| (input, Expr::Local(Box::new(ident), ty, Box::new(expr))))
    )(input)
}


/***************************************************************************
 * Main method
 ***************************************************************************/


/**
 * Main method, program starts here.
 */
fn main() {
    println!("{}", 2 - 3 * 5 + 11);
    let input = "let hello30: i32 = 1 + 2;";
    let result = parse_local(Span::new(input));
    
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
