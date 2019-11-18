
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

