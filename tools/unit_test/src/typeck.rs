
/***************************************************************************
 * Utilities for the type checking unit tests.
 ***************************************************************************/


use sqrrlc::sqrrlc::{
    session::Session,
    symbol:: {
        generator::*,
        table::SymbolTable,
    },
};


pub fn typeck_math(input: &str) -> IResult<TyKind> {
    let expr = Expr::parse(ParseSpan::new_extra(input, 0)).unwrap().1;
    
}


pub fn setup_tcx() -> TyCtxt {
    let sess = Session::new();
    let sym = gen_sym_table(&expr);
    TyCtxt::new(sess, sym)
}
