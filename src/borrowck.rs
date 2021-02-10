use std::collections::HashMap;
use crate::ast::*;
use crate::error::*;

/***************************************************************************
 * Borrow Checker Semantic Rules:
 * - Checks the lexical lifetime of local variables, so that
 *   there are no dangling references to dropped value.
 * - Lifetime is only based on the block that a particular
 *   variable is declared in.
 * - Lifetime represented by incrementing index, refers to
 *   particular block scope, when comparing lifetimes the
 *   lower index has always longer lifetime than higher index.
 * - Reference counting to ensure memory safety (two cases):
 *   - Multiple immutable references, no mutable references
 *   - One mutable reference, no immutable references
 ***************************************************************************/

struct BorrowContext<'a> {
    // tc: TypeContext<'a>,
    test: &'a u32, // FIXME(alexander): remove this, temporary to keep 'a, might need it in the future!
    scopes: Vec<BorrowScope>,
    next_lifetime: u32,
    error_count: u32
}

struct BorrowScope {
    locals: HashMap<Symbol, BorrowInfo>,
    lifetime: u32,
}

struct BorrowInfo {
    lifetime: u32,
    mutable_refs: u32,
    immutable_refs: u32,
    borrowed: bool,
}

fn push_borrow_scope<'a>(bc: &mut BorrowContext<'a>) -> u32 {
    let lifetime = bc.next_lifetime;
    let scope = BorrowScope {
        locals: HashMap::new(),
        lifetime,
    };
    bc.scopes.push(scope);
    bc.next_lifetime += 1;
    lifetime
}

fn pop_borrow_scope<'a>(bc: &mut BorrowContext<'a>) -> Option<BorrowScope> {
    bc.scopes.pop()
}

fn insert_borrow_info<'a>(bc: &mut BorrowContext<'a>, symbol: Symbol, lifetime: u32, borrowed: bool) {
    let len = bc.scopes.len();
    let info = BorrowInfo {
        lifetime,
        borrowed,
        mutable_refs: 0,
        immutable_refs: 0,
    };
    bc.scopes[len - 1].locals.insert(symbol, info);
}

fn get_mut_borrow_info<'a>(bc: &'a mut BorrowContext<'a>, symbol: &'a Symbol) -> &'a mut BorrowInfo {
    let len = bc.scopes.len();
    bc.scopes[len - 1].locals.get_mut(symbol).unwrap()
}

pub fn borrow_check_file<'a>(file: &'a File) -> u32 {
    let bc = &mut BorrowContext {
        test: &0,
        scopes: Vec::new(),
        next_lifetime: 0,
        error_count: 0,
    };

    for item in &file.items {
        match item {
            Item::Fn(func) => {
                let lifetime = push_borrow_scope(bc);
                for arg in func.decl.inputs {
                    
                    
                }
                
                borrow_check_block(bc, &func.block);
                pop_borrow_scope(bc);
            }

            _ => {}
        }
    }
    
    return bc.error_count;
}

fn borrow_check_block<'a>(bc: &mut BorrowContext<'a>, block: &'a Block) {
    
    let mut ret_info: Option<BorrowInfo> = None;
    for i in 0..block.stmts.len() {
        ret_info = borrow_check_stmt(block.stmts[i]);
    }

    bc.scopes.pop();
}

fn borrow_check_stmt<'a>(bc: &mut BorrowContext<'a>, stmt: &'a Stmt) -> Option<BorrowInfo> {
    match stmt {
        

    }
}

fn create_error_msg<'a>(
    bc: &BorrowContext<'a>,
    level: ErrorLevel,
    span: Span,
    message: &str,
    label: &str
) -> ErrorMsg {
    match bc.file {
        Some(file) => create_error_msg_from_span(level,
                                                 &file.lines,
                                                 span,
                                                 &file.filename,
                                                 &file.source,
                                                 message,
                                                 label),

        None => ErrorMsg {
            level: level,
            line_number: 0,
            column_number: 0,
            annotation_length: 0,
            path: "".to_string(),
            msg: message.to_string(),
            source: "".to_string(),
            label: label.to_string(),
            next: None,
        },
    }
}
