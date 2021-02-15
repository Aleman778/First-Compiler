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
    file: &'a File,
    scopes: Vec<BorrowScope>,
    next_lifetime: u32,
    error_count: u32
}

struct BorrowScope {
    locals: HashMap<Symbol, BorrowInfo>,
    lifetime: u32,
}

#[derive(Debug, Clone)]
struct BorrowInfo {
    ident: Symbol,
    lifetime: u32,
    mutable_refs: u32,
    immutable_refs: u32,
    borrowed_from: Option<Symbol>,
    declared_at: Span,
    used_at: Option<Span>,
}

fn push_borrow_scope<'a>(bc: &mut BorrowContext<'a>) {
    let lifetime = bc.next_lifetime;
    let scope = BorrowScope {
        locals: HashMap::new(),
        lifetime,
    };
    bc.scopes.push(scope);
    bc.next_lifetime += 1;
}

fn pop_borrow_scope<'a>(bc: &mut BorrowContext<'a>) -> Option<BorrowScope> {
    bc.scopes.pop()
}

fn insert_borrow_info<'a>(
    bc: &mut BorrowContext<'a>,
    symbol: Symbol,
    borrowed_from: Option<Symbol>,
    declared_at: Span
) {

    let len = bc.scopes.len();
    let info = BorrowInfo {
        ident: symbol,
        lifetime: bc.scopes[len - 1].lifetime,
        borrowed_from,
        declared_at,
        mutable_refs: 0,
        immutable_refs: 0,
        used_at: None,
    };
    bc.scopes[len - 1].locals.insert(symbol, info);
}

fn get_borrow_info<'a>(bc: &'a BorrowContext<'a>, symbol: &'a Symbol) -> &'a BorrowInfo {
    let len = bc.scopes.len();
    bc.scopes[len - 1].locals.get(symbol).unwrap()
}

fn get_mut_borrow_info<'a>(bc: &'a mut BorrowContext<'a>, symbol: &'a Symbol) -> &'a mut BorrowInfo {
    let len = bc.scopes.len();
    bc.scopes[len - 1].locals.get_mut(symbol).unwrap()
}

pub fn borrow_check_file<'a>(file: &'a File) -> u32 {
    let mut bc = BorrowContext {
        file,
        scopes: Vec::new(),
        next_lifetime: 0,
        error_count: 0,
    };

    for item in &file.items {
        match item {
            Item::Fn(func) => {
                bc.next_lifetime = 0;
                bc.scopes.clear();

                push_borrow_scope(&mut bc);
                for arg in &func.decl.inputs {
                    insert_borrow_info(&mut bc, arg.ident.sym, None, arg.span);
                }

                borrow_check_block(&mut bc, &func.block);
                pop_borrow_scope(&mut bc);
            }

            _ => {}
        }
    }

    return bc.error_count;
}

fn borrow_check_block<'a>(bc: &mut BorrowContext<'a>, block: &'a Block) {
    let mut ret_info: Option<BorrowInfo> = None;
    for i in 0..block.stmts.len() {
        ret_info = borrow_check_stmt(bc, &block.stmts[i]);

    }

    bc.scopes.pop();
}

fn borrow_check_stmt<'a>(bc: &mut BorrowContext<'a>, stmt: &'a Stmt) -> Option<BorrowInfo> {
    match stmt {
        Stmt::Local(local) => {
            if let Some(expr) = &*local.init {
                let borrow_info = borrow_check_expr(bc, expr);
                if let Some(info) = borrow_info {
                    let len = bc.scopes.len();
                    bc.scopes[len - 1].locals.insert(local.ident.sym, info);
                } else {
                    insert_borrow_info(bc, local.ident.sym, None, local.ident.span);
                }
            }

            None
        }

        Stmt::Semi(expr) => borrow_check_expr(bc, expr),
        Stmt::Expr(expr) => borrow_check_expr(bc, expr),

        _ => None,
    }
}

fn borrow_check_expr<'a>(bc: &mut BorrowContext<'a>, expr: &'a Expr) -> Option<BorrowInfo> {
    match expr {
        Expr::Assign(assign) => {
            None
        }

        Expr::Ident(ident) => {
            let len = bc.scopes.len();
            let borrow_info = bc.scopes[len - 1].locals.get_mut(&ident.sym).unwrap();
            if let None = borrow_info.used_at {
                borrow_info.used_at = Some(ident.span);
            }
            // get_borrow_info(bc, &ident.sym);
            // TODO(alexander): do we need to return anything here?
            None
        }

        Expr::Return(ret) => {
            if let Some(expr) = &*ret.expr {
                borrow_check_expr(bc, &expr)
            } else {
                None
            }
        }

        Expr::Reference(expr) => {
            None

        }

        _ => None,
    }
}

fn create_error_msg<'a>(
    bc: &BorrowContext<'a>,
    level: ErrorLevel,
    span: Span,
    message: &str,
    label: &str
) -> ErrorMsg {
    create_error_msg_from_span(level, &bc.file.lines, span, &bc.file.filename, &bc.file.source, message, label)
}
