
/***************************************************************************
 * Parser module is responsible for parsing source code and transforming
 * it into a data structure known as an Abstract Syntax Tree (AST).
 * The AST data structure is implemented in the AST module.
 ***************************************************************************/


/**
 * Parser trait defines a generic parser that should
 * be implemented by each structure of the AST.
 */
trait Parser<T> {
    fn parse(input: Span) -> IResult<Span, T>;
}


impl Parser<Atom> for Atom {
    fn parse(input: Span) -> IResult<Span, Atom> {
        
    }
}
