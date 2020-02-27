//! Macros for more easily creating unit tests for the parser.

#[macro_export]
macro_rules! parse_expr {
    ($source:expr) => (
        let ctx = create_parse_ctx!($source);
        parse_expr(&mut ctx)
    );
}


#[macro_export]
macro_rules! parse_lit {
    ($source:expr) => (
        if let ExprKind::Lit(lit) = parse_expr!($source) {
            *lit.kind
        } else {
            panic!("not a literal expression")
        }
    );
}
