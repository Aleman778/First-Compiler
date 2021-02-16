use std::collections::HashMap;
use crate::ast::*;
use crate::error::*;

/***************************************************************************
 * Borrow Checker Semantic Rules:
 * - Checks the lexical lifetime of local variables, so that
 *   there are no references to any dropped value.
 * - Lifetime is only based on the block that a particular
 *   variable is declared in.
 * - Lifetime represented by incrementing index, refers to
 *   particular block scope, when comparing lifetimes the
 *   lower index has always longer lifetime than higher index.
 * - Reference counting to ensure memory safety (two cases):
 *   - Multiple immutable references, no mutable references
 *   - One mutable reference, no immutable references
 * - There is no concept of move semantics in this borrow checker
 *   since all types are primitives they get copied instead of moved.
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
            Some(bc.scopes[len - 1].locals.get(&ident.sym).unwrap().clone())
        }

        Expr::Return(ret) => {
            if let Some(expr) = &*ret.expr {
                borrow_check_expr(bc, &expr)
            } else {
                None
            }
        }

        Expr::Reference(expr) => {
            let mut borrow_info = borrow_check_expr(bc, &*expr.expr);
            if let Some(info) = &mut borrow_info {
                
                // Count owners references
                let len = bc.scopes.len();
                let owner = bc.scopes[len - 1].locals.get_mut(&info.ident).unwrap();
                
                info.borrowed_from = Some(owner.ident);
                if expr.mutable {
                    owner.mutable_refs += 1;
                } else {
                    owner.immutable_refs += 1;
                }

                let mutable_refs = owner.mutable_refs;
                let immutable_refs = owner.immutable_refs;
                let owner_ident = owner.ident;
                // println!("borrow_info({}) = {:#?}", resolve_symbol(owner.ident), owner);


                if expr.mutable {
                    // NOTE(alexander): - One mutable reference, no immutable references
                    if mutable_refs > 1 {
                        let err_msg = create_error_msg(
                            bc, ErrorLevel::Error, expr.span,
                            &format!("cannot borrow `{}` as mutable more than once",
                                     resolve_symbol(owner_ident)),
                            "immutable borrow occurs here");
                        print_error_msg(&err_msg);
                        bc.error_count += 1;
                        
                    } else if immutable_refs > 0 {
                        let err_msg = create_error_msg(
                            bc, ErrorLevel::Error, expr.span,
                            &format!("cannot borrow `{}` as mutable because it is also borrowed as immutable",
                                     resolve_symbol(owner_ident)),
                            "immutable borrow occurs here");
                        print_error_msg(&err_msg);
                        bc.error_count += 1;
                    }
                } else {

                    // NOTE(alexander): - Multiple immutable references, no mutable references
                    if mutable_refs > 0 {
                        let err_msg = create_error_msg(
                            bc, ErrorLevel::Error, expr.span,
                            &format!("cannot borrow `{}` as immutable because it is also borrowed as mutable",
                                     resolve_symbol(owner_ident)),
                            "immutable borrow occurs here");
                        print_error_msg(&err_msg);
                        bc.error_count += 1;
                    }
                }
            }

            borrow_info
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
