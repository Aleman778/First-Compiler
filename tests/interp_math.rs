
/***************************************************************************
 * Tests for interpreting mathematical expressions,
 * incl operator precedence.
 ***************************************************************************/


use unit_test::interp::*;


#[test]
fn interp_lit_int() {
    assert_eq!(interp_math("42").unwrap().get_i32(), Some(42));
}


#[test]
fn interp_lit_bool() {
    assert_eq!(interp_math("true").unwrap().get_bool(), Some(true));
}


/***************************************************************************
 * Binary Operations
 ***************************************************************************/


#[test]
fn interp_add() {
    assert_eq!(interp_math("1 + 2 + 3 + 4 + 5").unwrap().get_i32(), Some(15));
}


#[test]
fn interp_sub() {
    assert_eq!(interp_math("10 - 5 - 2 - 1").unwrap().get_i32(), Some(2));
}


#[test]
fn interp_mul() {
    assert_eq!(interp_math("2*3*1*2*5").unwrap().get_i32(), Some(60));
}


#[test]
fn interp_div() {
    assert_eq!(interp_math("1000/10/2/25").unwrap().get_i32(), Some(2));
}


#[test]
fn interp_pow() {
    assert_eq!(interp_math("2**3**2**1").unwrap().get_i32(), Some(512));
}


#[test]
fn interp_mod() {
    assert_eq!(interp_math("37%10%3").unwrap().get_i32(), Some(1));
}

#[test]
fn interp_eq() {
    assert_eq!(interp_math("100 == 100").unwrap().get_bool(), Some(true));
    assert_eq!(interp_math("100 == 53").unwrap().get_bool(), Some(false));
    assert_eq!(interp_math("false == false").unwrap().get_bool(), Some(true));
    assert_eq!(interp_math("true == false").unwrap().get_bool(), Some(false));
}

#[test]
fn interp_ne() {
    assert_eq!(interp_math("100 != 100").unwrap().get_bool(), Some(false));
    assert_eq!(interp_math("100 != 53").unwrap().get_bool(), Some(true));
    assert_eq!(interp_math("false != false").unwrap().get_bool(), Some(false));
    assert_eq!(interp_math("true != false").unwrap().get_bool(), Some(true));
}

#[test]
fn interp_lt() {
    assert_eq!(interp_math("32 < 50").unwrap().get_bool(), Some(true));
    assert_eq!(interp_math("32 < 32").unwrap().get_bool(), Some(false));
}


#[test]
fn interp_le() {
    assert_eq!(interp_math("32 <= 50").unwrap().get_bool(), Some(true));
    assert_eq!(interp_math("32 <= 32").unwrap().get_bool(), Some(true));
}


#[test]
fn interp_gt() {
    assert_eq!(interp_math("32 > 32").unwrap().get_bool(), Some(false));
    assert_eq!(interp_math("32 > 25").unwrap().get_bool(), Some(true));
}


#[test]
fn interp_ge() {
    assert_eq!(interp_math("32 >= 32").unwrap().get_bool(), Some(true));
    assert_eq!(interp_math("32 > 25").unwrap().get_bool(), Some(true));
}


/***************************************************************************
 * Unary operations
 ***************************************************************************/


#[test]
fn interp_neg() {
    assert_eq!(interp_math("-32").unwrap().get_i32(), Some(-32));
}


#[test]
fn interp_not() {
    assert_eq!(interp_math("!false").unwrap().get_bool(), Some(true));
}
