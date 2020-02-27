
/***************************************************************************
 * The symbols sub module provides symbols and a symbol table for
 * storing information about symbols that can be used for analysis.
 ***************************************************************************/


use std::fmt;
use crate::ast::Ty;


/**
 * Symbol is a simple entry in the symbol table.
 * Each symbol has a kind and entries in the table.
 */
#[derive(Debug)]
pub enum Symbol {
    /// Variable symbols.
    Var(VarSymbol),

    /// Function declaration symbol.
    Fn(FnSymbol),
}


/**
 * Variable symbol holds data about a specific variable.
 */
pub struct VarSymbol {
    /// The type of this specific variable.
    pub ty: Ty,
}


/**
 * Function symbol holds data about a spcific function declaration.
 */
pub struct FnSymbol {
    /// The types that this function takes in as arguments.
    pub inputs: Vec<Ty>,

    /// The output type that this function returns.
    pub output: Ty,
}


/**
 * Implementation of variable symbol struct.
 */
impl VarSymbol {
    /**
     * Creates a new variable symbol.
     */
    pub fn new() -> Self {
        VarSymbol {
            ty: Ty::new(),
        }
    }
}


/**
 * Implementation of function declaration symbol struct.
 */
impl FnSymbol {
    /**
     * Creates a new function declaration symbol.
     */
    pub fn new() -> Self {
        FnSymbol {
            inputs: Vec::new(),
            output: Ty::new(),
        }
    }
    

    /**
     * Convert input types to csv string e.g. i32,i64,bool.
     */
    pub fn input_to_string(&self) -> String {
        let mut result = String::new();
        for ty in &self.inputs {
            result.push_str(format!("{},", ty).as_str());
        }
        result.pop();
        result
    }
}


/**
 * Debug formatting for variable symbols.
 */
impl fmt::Debug for VarSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VarSymbol")
            .field("type", &format_args!("{}", self.ty))
            .finish()
    }
}


/**
 * Debug formatting for function symbols.
 */
impl fmt::Debug for FnSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FnSymbol")
            .field("inputs", &format_args!("[{}]", self.input_to_string()))
            .field("output", &format_args!("{}", self.output))
            .finish()
    }
}


pub mod table;
pub mod generator;
