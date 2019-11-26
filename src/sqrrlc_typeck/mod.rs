
/***************************************************************************
 * Type checker is a sub module of the semantic analyser and is used
 * check the program for type related errors.
 ***************************************************************************/


use crate::sqrrlc::session::Session;
use crate::sqrrlc::symbol::table::SymbolTable;
use crate::sqrrlc_ast::ty::Ty;


/**
 * Type context is used to hold information used by the type cheker.
 */
pub struct TyCtxt<'tcx> {
    pub sess: &'tcx Session,
    pub sym: &'tcx mut SymbolTable,
}


impl<'tcx> TyCtxt<'tcx> {
    /**
     * Creates a new type context with the given
     * compiler session and symbol table to use.
     */
    pub fn new(sess: &'tcx Session, sym: &'tcx mut SymbolTable) -> Self {
        TyCtxt{sess, sym}
    }
}


/**
 * The type checker trait should be implemented for every AST node
 * that handles types and can generate a type error.
 */
pub trait TypeChecker {
    fn check_type(&self, tcx: &mut TyCtxt) -> Ty;
}


pub mod error;
pub mod base;
pub mod stmt;
pub mod expr;
pub mod lit;
pub mod op;
