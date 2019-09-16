#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod expr;
mod func;

fn main() {
    println!("Hello, world!");
}

#[test]
fn test_term_parser() {
    assert!(grammar::TermParser::new().parse("22").is_ok());
    assert!(grammar::TermParser::new().parse("(22)").is_ok());
    assert!(grammar::TermParser::new().parse("((((22))))").is_ok());
    assert!(grammar::TermParser::new().parse("((22)").is_err());
}
