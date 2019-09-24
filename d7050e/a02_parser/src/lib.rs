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
    bytes::complete::{tag, take_while1},
    character::complete::{digit1, alpha1, multispace0, multispace1},
    character::{is_alphanumeric},
    combinator::{map, peek, opt, cut},
    branch::alt,
    multi::{fold_many0, separated_list},
    sequence::{pair, tuple, preceded, terminated},
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
pub type Span<'a> = LocatedSpan<&'a str>;


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
    SyntaxError,
}


/**
 * Implement nom parse error functions for Error.
 */
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

    // Parentheses
    Paren(Box<SpanExpr<'a>>),
    
    // Local variable declaration
    Local(bool, Box<SpanExpr<'a>>, SpanType<'a>, Box<SpanExpr<'a>>),

    // Assignment of mutable local variable
    Assign(Box<SpanExpr<'a>>, Box<SpanExpr<'a>>),

    // Function call
    Call(Box<SpanExpr<'a>>, Vec<SpanExpr<'a>>),

    // Block of expressions
    Block(Vec<SpanExpr<'a>>),

    // If statement with optional else statement
    If(Box<SpanExpr<'a>>, Box<SpanExpr<'a>>, Box<Option<SpanExpr<'a>>>),

    // While loop
    While(Box<SpanExpr<'a>>, Box<SpanExpr<'a>>),

    // Break loop
    Break(),
    
    // Continue next loop cycle
    Continue(),
        
    // Return expression
    Return(Box<Option<SpanExpr<'a>>>),
    
    // Identifier
    Ident(&'a str),

    // Value literal e.g. i32 or bool value.
    Val(Val),
}


/**
 * Type alias of expressions to include span.
 */
pub type SpanExpr<'a> = (Span<'a>, Expr<'a>);


/**
 * Value enum contains either a i32 or bool value.
 */
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Val {
    // Number literal
    Num(i32),

    // Boolean literal
    Bool(bool),
}


/**
 * Type alias of value to include span.
 */
pub type SpanVal<'a> = (Span<'a>, Val);


/**
 * Argument struct defines an identifier with a type
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Argument<'a>(pub SpanExpr<'a>, pub SpanType<'a>);


/**
 * Type alias of argument to include span.
 */
pub type SpanArg<'a> = (Span<'a>, Argument<'a>);

    
/**
 * Function struct defines the functions characteristics,
 * i.e. function name identifier, vector of arguments, the return type
 * and the block containing the function implementation.
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a>(pub Box<SpanExpr<'a>>, pub Vec<SpanArg<'a>>, pub Option<SpanType<'a>>, pub Box<SpanExpr<'a>>);


/**
 * Type alias of function to include span.
 */
pub type SpanFn<'a> = (Span<'a>, Function<'a>);


/**
 * Root of the Abstract Syntax Tree, contains different itesm
 * currently I only support functions. So this defines a vector
 * of functions defined in a source file.
 */
#[derive(Debug, PartialEq)]
pub struct AST<'a>(pub Vec<SpanFn<'a>>);


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
pub type SpanOp<'a> = (Span<'a>, Op);


/**
 * The different kinds of supported types.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int32,
    Bool,
}


/**
 * Type alias of the type enum to include span.
 */
type SpanType<'a> = (Span<'a>, Type);


/***************************************************************************
 * Addition arithmetic expression parser.
 * Note: operator precedence is considered for this assignment.
 ***************************************************************************/


/**
 * Parses a simple arithmetic expression, only supports addition.
 */
pub fn parse_expr(input: Span) -> IResult<Span, SpanExpr> {
    alt((
        // Binary operation e.g. 4 + 5
        map(tuple((parse_atom_ms, preceded(multispace0, parse_binoperator), parse_expr_ms)),
            |(left, op, right)| (input, Expr::BinOp(Box::new(left), op, Box::new(right)))),
        
        // Unary operation e.g. -5
        map(tuple((preceded(multispace0, parse_unoperator), parse_expr_ms)),
            |(op, right)| (input, Expr::UnOp(op, Box::new(right)))),
        
        // Local variable declaration e.g. let a: i32 = 5;
        parse_local,

        // Assignment of local variables.
        parse_assign,

        // Parse if statement e.g. if <expr> { <then-body> } else { <else-body> }
        parse_if,

        // Parse while loops e.g. while <expr> { <body> }
        parse_while,

        // Parse a block of expressions e.g. { <stmt_1>; ... <stmt_n> }
        parse_block,

        // Parse keywords i.e. return, continue and break
        parse_keywords,

        // Parse an operand (literal, function call or identifier)
        parse_atom,
    ))(input)
}


/**
 * Parses an expression with preceded by spaces.
 */
pub fn parse_expr_ms(input: Span) -> IResult<Span, SpanExpr> {
    preceded(multispace0, parse_expr)(input)
}


/**
 * Parses an operand can either be a literal, identifier or function call.
 */
pub fn parse_atom(input: Span) -> IResult<Span, SpanExpr> {
    alt((
        // Parentheses
        parse_paren,
        
        // Literal e.g. false
        parse_literal,

        // Function calls e.g. min(a + 25, 30);
        parse_fn_call,        

        // Parse an identifier e.g. a
        parse_identifier,
    ))(input)
}


/**
 * Parses an operand with possible multispaces before.
 */
pub fn parse_atom_ms(input: Span) -> IResult<Span, SpanExpr> {
    preceded(multispace0, parse_atom)(input)
}


/**
 * Parse a binary operator.
 */
pub fn parse_binoperator(input: Span) -> IResult<Span, SpanOp> {
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
pub fn parse_unoperator(input: Span) -> IResult<Span, SpanOp> {
    alt((
        map(tag("-"),  |s| (s, Op::Sub)),
        map(tag("!"),  |s| (s, Op::Not)),
    ))(input)
}


/**
 * Parse parentheses e.g. (2 * (3 + 5))
 */
pub fn parse_paren(input: Span) -> IResult<Span, SpanExpr> {
    map(tuple((
        tag("("),
        preceded(multispace0, parse_expr),
        preceded(multispace0, tag(")")),
    )),
        |(_, expr, _)| (input, Expr::Paren(Box::new(expr)))
    )(input)
}


/**
 * Parses a 32-bit signed integer.
 * Also handles errors this is not a valid i32 number.
 * Note: taken from the instructors example code.
 */
pub fn parse_i32<'a>(input: Span<'a>) -> IResult<Span<'a>, SpanExpr> {
    let (input, digits) = digit1(input)?;
    match digits.fragment.parse() {
        Ok(n) => Ok((input, (digits, Expr::Val(Val::Num(n))))),
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
        map(tag("true"), |s| (s, Expr::Val(Val::Bool(true)))),
        map(tag("false"), |s| (s, Expr::Val(Val::Bool(false)))),
    ))(input)
}


/**
 * Parse a type, e.g. i32 or bool.
 */
pub fn parse_type(input: Span) -> IResult<Span, SpanType> {
    alt((
        map(tag("i32"),  |s| (s, Type::Int32)),
        map(tag("bool"),  |s| (s, Type::Bool)),
    ))(input)
}


/**
 * Parses an identifier, has to start with an alphabetic character.
 * e.g. to_string, testVar, hello30.
 */
pub fn parse_identifier(input: Span) -> IResult<Span, SpanExpr> {
    peek(alpha1)(input)?;
    map(take_while1(|c: char| is_alphanumeric(c as u8) || c == '_'),
        |s: Span| (s, Expr::Ident(s.fragment))
    )(input)
}


/**
 * Parse a function call. E.g. min(a + 5, 30).
 */
pub fn parse_fn_call(input: Span) -> IResult<Span, SpanExpr> {
    map(tuple((
        parse_identifier,
        preceded(multispace0, tag("(")),
        separated_list(
            preceded(multispace0, tag(",")),
            preceded(multispace0, parse_expr)
        ),
        preceded(multispace0, cut(tag(")"))),
        peek(preceded(multispace0, tag(";"))),
    )),
        |(ident, _, args, _, _)| {
            (input, Expr::Call(Box::new(ident), args))
        }
    )(input)
}


/**
 * Parse a function declaration. E.g. fn min(a: i32, b: i32) -> i32 <parse_block>.
 */
pub fn parse_func_decl(input: Span) -> IResult<Span, SpanFn> {
    preceded(
        tag("fn"),
        map(tuple((
            preceded(multispace0, parse_identifier),
            preceded(multispace0, tag("(")),
            separated_list(
                preceded(multispace0, tag(",")),
                preceded(multispace0, parse_func_arg)
            ),
            preceded(multispace0, cut(tag(")"))),
            opt(preceded(pair(multispace0, tag("->")), preceded(multispace0, parse_type))),
            preceded(multispace0, parse_block),
        )),
            |(ident, _, args, _, ret_type, block)| {
                (input, Function(Box::new(ident), args, ret_type, Box::new(block)))
            }
        )
    )(input)
}


/**
 * Parse a function argument, e.g. var: i32.
 */
pub fn parse_func_arg(input: Span) -> IResult<Span, SpanArg> {
    map(tuple((
        parse_identifier,
        preceded(multispace0, tag(":")),
        preceded(multispace0, parse_type),
    )),
        |(ident, _, ty)| (input, Argument(ident, ty))
    )(input)
}


/**
 * Parse assignment of mutable local variables
 */
pub fn parse_assign(input: Span) -> IResult<Span, SpanExpr> {
    map(tuple((
        parse_identifier,
        preceded(multispace0, tag("=")),
        preceded(multispace0, parse_expr),
        peek(preceded(multispace0, tag(";"))),
    )),
        |(ident, _, expr, _)| (input, Expr::Assign(Box::new(ident), Box::new(expr)))
    )(input)
}


/**
 * Parse declarations of local variables
 * e.g. let a: i32 = 5 + 7;
 */
pub fn parse_local(input: Span) -> IResult<Span, SpanExpr> {
    preceded(
        tag("let"),
        map(tuple((
            opt(preceded(multispace1, tag("mut"))),
            preceded(multispace1, parse_identifier),
            preceded(multispace0, tag(":")),
            preceded(multispace0, parse_type),
            preceded(multispace0, tag("=")),
            preceded(multispace0, parse_expr),
            peek(preceded(multispace0, tag(";"))),
        )),
            |(mutable, ident, _, ty, _, expr, _)| {
                (input, Expr::Local(mutable.is_some(), Box::new(ident), ty, Box::new(expr)))
            }
        )
    )(input)
}


/**
 * Parses a block containing multiple expressions.
 * E.g.  {
 *           let mut a: i32 = 5;
 *           let b: i32 = 8;
 *           a = add(a + b);
 *       }
 * Blocks can also be recusive e.g. { { <expr1> } { <expr2> } }
 */
pub fn parse_block(input: Span) -> IResult<Span, SpanExpr> {
    preceded(
        tag("{"),
        map(tuple((
            fold_many0(
                preceded(
                    multispace0,
                    terminated(
                        parse_expr,
                        preceded(
                            multispace0, alt((
                                tag(";"),
                                peek(tag("}")),
                            ))
                        )
                    )
                ),
                Vec::new(),
                |mut stmts: Vec<_>, expr| {
                    stmts.push(expr);
                    stmts
                }
            ),
            preceded(multispace0, tag("}")),
        )),
        |(stmts, _)| (input, Expr::Block(stmts)))
    )(input)
}


/**
 * Parse if statement with optional else statement
 * e.g. if a > 10 { then-statements; } else { else-statements; }
 */
pub fn parse_if(input: Span) -> IResult<Span, SpanExpr> {
    preceded(
        tag("if"),
        map(tuple((
            preceded(multispace1, parse_expr),
            preceded(multispace0, parse_block),
            opt(preceded(
                pair(multispace0, tag("else")),
                preceded(multispace0, parse_block))
            ),
        )),
            |(cond, then_body, else_body)| {
                (input, Expr::If(Box::new(cond), Box::new(then_body), Box::new(else_body)))
            }
        )
    )(input)
}


/**
 * Parse while statement. E.g. while i < 10 { foo(); i = i + 1; }
 * Supports keywords break and continue.
 */
pub fn parse_while(input: Span) -> IResult<Span, SpanExpr> {
    preceded(
        tag("while"),
        map(tuple((
            preceded(multispace1, parse_expr),
            preceded(multispace0, parse_block),
        )),
            |(cond, body)| {
                (input, Expr::While(Box::new(cond), Box::new(body)))
            }
        )
    )(input)
}


/**
 * Parse simple keywords such as return, continue and break.
 */
pub fn parse_keywords(input: Span) -> IResult<Span, SpanExpr> {
    alt((
        map(preceded(pair(tag("return"), multispace1), opt(parse_expr)), |expr| (input, Expr::Return(Box::new(expr)))),
        map(tag("break"), |_| (input, Expr::Break())),
        map(tag("continue"), |_| (input, Expr::Continue())),
    ))(input)
}


/**
 * Parses the input string and produces an AST.
 */
pub fn parse<'a>(input: &'a str) -> Result<AST<'a>, Err<Error<'a>>>{
    let span = Span::new(input);
    let parser = fold_many0(
        preceded(multispace0, parse_func_decl),
        Vec::new(),
        |mut functions: Vec<_>, function| {
            functions.push(function);
            functions
        }
    );
    match parser(span) {
        Ok(a) => Ok(AST(a.1)),
        Err(_e) => Err(Err::Error(Error(
            span,
            None,
            ErrorKind::SyntaxError,
        ))),
    }
}
