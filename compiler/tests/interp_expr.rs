
/***************************************************************************
 * Tests for interpreting expressions.
 ***************************************************************************/


use utilities::interp::*;
use compiler::interp::value::Val;


#[test]
fn interp_assign() {
    assert_eq!(
        interp_expr("{let mut a: i32 = 5; a = a + a * 2; a}").unwrap(),
        val_i32(15)
    );
}


#[test]
fn interp_block() {
    assert_eq!(
        interp_expr("{ let a: i32 = 5; { { a } } }").unwrap(),
        val_i32(5)
    );
}


#[test]
fn interp_break() {
    assert_eq!(
        interp_expr("break;").unwrap(),
        val_break()
    );

    assert!(
        interp_expr("while true { break; }").is_ok()
    );
}


#[test]
fn interp_call() {
    let program = "fn sqr(a: i32) -> i32 { a * a } fn inv(b: bool) -> bool { !b }";
    assert_eq!(
        interp_file(program, "sqr(9);").unwrap(),
        val_i32(81)
    );
        
    assert_eq!(
        interp_file(program, "inv(false);").unwrap(),
        val_bool(true)
    );
}


#[test]
fn interp_continue() {
    assert_eq!(
        interp_expr("continue;").unwrap(),
        val_continue()
    );

    assert!(
        interp_expr("{ let mut a: i32 = 1; while a > 0 { a = a - 1; continue; a = a + 5; } }").is_ok()
    );
}



#[test]
fn interp_ident() {
    assert_eq!(
        interp_expr("{ let a: i32 = 10; a + a }").unwrap(),
        val_i32(20)
    );
}


#[test]
fn interp_if() {
    assert_eq!(
        interp_expr("if 5 < 10 { true } else { false }").unwrap(),
        val_bool(true)
    );
    
    assert_eq!(
        interp_expr("if 10 < 5 { true } else { false }").unwrap(),
        val_bool(false)
    );

    assert_eq!(
        interp_expr("if 10 < 5 { true }").unwrap(),
        Val::None
    );
}


#[test]
fn interp_local() {
    assert_eq!(
        interp_expr("{ let a: i32 = 32; a }").unwrap(),
        val_i32(32)
    );
    
    assert_eq!(
        interp_expr("{ let a: bool = false; a }").unwrap(),
        val_bool(false)
    );
    
    assert!(
        interp_expr("{ let mut a: i32 = 32; a = a + 5; }").is_ok()
    );
        
    // assert!(
        // interp_expr("{ let a: i32 = 32; a = a + 5; }").is_err()
    // );
}


#[test]
fn interp_paren() {
    assert_eq!(
        interp_math("(5 * (3 + 2))").unwrap(),
        val_i32(25)
    );
}


#[test]
fn interp_return() {
    assert_eq!(
        interp_expr("{ return 5; }").unwrap(),
        val_i32(5)
    );
    
    assert_eq!(
        interp_expr("{ 5 }").unwrap(),
        val_i32(5)
    );
}

#[test]
fn interp_while() {
    assert_eq!(
        interp_expr("{ let mut i: i32 = 5; while i < 10 { i = i + 1; } i }").unwrap(),
        val_i32(10)
    );
}
