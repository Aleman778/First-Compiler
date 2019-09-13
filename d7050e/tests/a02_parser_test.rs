extern crate a02_parser;

    
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
