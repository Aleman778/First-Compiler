
/***************************************************************************
 * Simple macros for simplifying creation of error diagnostics.
 ***************************************************************************/


#[macro_export]
macro_rules! span_warn {
    ($session:expr, $span:expr, $($message:tt)*) => ({
        $session.span_warn(
            $span,
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! span_err {
    ($session:expr, $span:expr, $($message:tt)*) => ({
        $session.span_err(
            $span,
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! span_fatal {
    ($session:expr, $span:expr, $($message:tt)*) => ({
        $session.span_fatal(
            $span,
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! struct_span_warn {
    ($session:expr, $span:expr, $($message:tt)*) => ({
        $session.struct_span_warn(
            $span,
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! struct_span_err {
    ($session:expr, $span:expr, $($message:tt)*) => ({
        $session.struct_span_err(
            $span,
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! struct_span_fatal {
    ($session:expr, $span:expr, $($message:tt)*) => ({
        $session.struct_span_fatal(
            $span,
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! struct_warn {
    ($session:expr, $($message:tt)*) => ({
        $session.struct_warn(
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! struct_err {
    ($session:expr, $($message:tt)*) => ({
        $session.struct_err(
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! struct_fatal {
    ($session:expr, $($message:tt)*) => ({
        $session.struct_fatal(
            &format!($($message)*)
        )
    })
}


#[macro_export]
macro_rules! mismatched_types_err {
    ($session:expr, $span:expr, $expected:expr, $found:expr) => ({
        let mut err = $session.struct_span_err($span, "mismatched types");
        err.span_label($span, &format!("expected `{}`, found `{}`", $expected, $found));
        $session.emit(&err);
    })
}

#[macro_export]
macro_rules! mismatched_return_type_err {
    ($session:expr, $fn_sym:expr, $ret_ty:expr) => ({
        let mut err = $session.struct_span_err($ret_ty.span, "mismatched types");
        err.span_label($ret_ty.span, &format!("expected `{}`, found `{}`", $fn_sym.output, $ret_ty));
        if $fn_sym.output.is_none() {
            err.span_label($fn_sym.output.span, "possibly return type missing here?");
        } else {
            err.span_label($fn_sym.output.span, &format!(
                "expected `{}` because of return type", $fn_sym.output));
        }
        $session.emit(&err);
    })
}

#[macro_export]
macro_rules! mismatched_types_fatal {
    ($session:expr, $span:expr, $expected:expr, $found:expr) => ({
        let mut err = $session.struct_span_fatal($span, "mismatched types");
        err.span_label($span, &format!("expected `{}`, found `{}`", $expected, $found));
        err
    })
}
