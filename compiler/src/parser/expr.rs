
/***************************************************************************
 * Expression parser implementation defines parsers for any
 * type of expression such as let statements, binary operations etc.
 ***************************************************************************/


use nom::{
    branch::alt,
    error::context,
};


use crate::ast::{
    expr::*,
};


use crate::parser::{
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

