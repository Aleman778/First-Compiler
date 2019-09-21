
/***************************************************************************
 * Atoms are basic structures such as parenthesized expressions,
 * literals, function calls and identifiers. This is used for example to
 * prevent infinite left recursion when parsing binary operations.
 ***************************************************************************/


/**
 * Atom enum contains different types of values used
 * in expressions e.g. integers, bools etc.
 */
pub enum Atom<'a> {
    Paren(Paren<'a>),
    LitInt(LitInt<'a>),
    LitBool(LitBool<'a>),
    Ident(Ident<'a>),
}


/**
 * Parenthesized expressions.
 */
pub struct Paren<'a> {
    expr: Expr<'a>,
    span: Span<'a>,
}


/**
 * Literal integer struct has an i32 value.
 */
pub struct LitInt<'a> {
    value: i32,
    span: Span<'a>,
}


/**
 * Literal boolean struct has a bool value.
 */
pub struct LitBool<'a> {
    value: bool,
    span: Span<'a>,
}


/**
 * Function call contains the identifier and arguments.
 */
pub struct FnCall<'a> {
    ident: Ident<'a>,
    args: Vec<Expr<'a>>,
    span: Span<'a>,
}


/**
 * Identifier struct contains a user defined name.
 */
pub struct Ident<'a> {
    to_string: &'a str,
    span: Span<'a>,
}
