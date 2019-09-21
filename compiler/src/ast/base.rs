
/**
 * File struct is the root of the AST in a source file.
 * The file structure contains a vector of items. Currently
 * only function items are supported, but can easily be
 * extended to support any item such as structs, type alias etc.
 */
pub struct File<'a> {
    items: Vec<Item<'a>>,
    span: Span<'a>
}


/**
 * Items enum contains all types of items that appear in a file.
 * This currently only supports item functions.
 */
pub enum Item<'a> {
    ItemFn(ItemFn<'a>),
}


/**
 * 
 */
pub struct ItemFn<'a> {
    ident: Ident<'a>,
    decl: FnDecl<'a>,
    block: Expr<'a>,
    span: Span<'a>,
}


/**
 * Function declaration struct contains information about the
 * functions input arguments and the output type.
 */
pub struct FnDecl<'a> {
    inputs: Vec<Argument<'a>>,
    output: Type<'a>,
    span: Span<'a>,
}


/**
 * Argument struct contains an identifier and a type.
 */
pub struct Argument<'a> {
    ident: Ident<'a>,
    ty: Type<'a>,
    span: Span<'a>,
}


/**
 * Type enum currently only supports i32 and bool.
 */
pub enum Type<'a> {
    Int32(Int32<'a>),
    Bool(Bool<'a>)
}


/**
 * 32 bit signed integer type.
 */
struct Int32 <'a> {
    span: Span<'a>,
};


/**
 * Boolean type.
 */
struct Bool<'a> {
    span: Span<'a>,
};
