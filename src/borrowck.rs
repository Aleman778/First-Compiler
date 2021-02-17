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
    temp_symbol: Symbol,
    temp_index: u32,
    next_lifetime: u32,
    error_count: u32
}

struct BorrowScope {
    locals: HashMap<Ident, BorrowInfo>,
    lifetime: u32,
}

#[derive(Debug, Clone)]
struct BorrowInfo {
    ident: Ident,
    lifetime: u32,
    mutable_refs: u32,
    immutable_refs: u32,
    borrowed_from: Option<Ident>,
    declared_at: Span,
    used_at: Option<Span>,
    from_return: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Ident {
    pub symbol: Symbol,
    pub index: u32, // used to distinguish identifiers with the same symbol.
}

fn push_borrow_scope<'a>(bc: &mut BorrowContext<'a>) {
    let lifetime = bc.next_lifetime;
    let len = bc.scopes.len();
    let locals = if len > 0 {
        bc.scopes[len - 1].locals.clone()
    } else {
        HashMap::new()
    };
    
    let scope = BorrowScope {
        locals,
        lifetime,
    };

    bc.scopes.push(scope);
    bc.next_lifetime += 1;
}

fn pop_borrow_scope<'a>(bc: &mut BorrowContext<'a>) -> Option<BorrowScope> {
    bc.scopes.pop()
}


fn insert_temp_borrow_info<'a>(
    bc: &mut BorrowContext<'a>,
    borrowed_from: Option<Ident>,
    declared_at: Span
) -> BorrowInfo {
    
    let len = bc.scopes.len();
    let ident = Ident {
        symbol: bc.temp_symbol,
        index: bc.temp_index,
    };
    
    let info = BorrowInfo {
        ident,
        lifetime: bc.scopes[len - 1].lifetime,
        borrowed_from,
        declared_at,
        mutable_refs: 0,
        immutable_refs: 0,
        used_at: None,
        from_return: false,
    };
    
    bc.scopes[len - 1].locals.insert(ident, info.clone());
    bc.temp_index += 1;
    info
}

fn insert_borrow_info<'a>(
    bc: &mut BorrowContext<'a>,
    symbol: Symbol,
    borrowed_from: Option<Ident>,
    declared_at: Span
) {

    let len = bc.scopes.len();
    let ident = Ident { symbol, index: 0 };
    let info = BorrowInfo {
        ident,
        lifetime: bc.scopes[len - 1].lifetime,
        borrowed_from,
        declared_at,
        mutable_refs: 0,
        immutable_refs: 0,
        used_at: None,
        from_return: false,
    };
    bc.scopes[len - 1].locals.insert(ident, info);
}

fn get_borrow_info<'a>(bc: &'a BorrowContext<'a>, ident: &'a Ident) -> &'a BorrowInfo {
    let len = bc.scopes.len();
    bc.scopes[len - 1].locals.get(ident).unwrap()
}

fn get_mut_borrow_info<'a>(bc: &'a mut BorrowContext<'a>, ident: &'a Ident) -> &'a mut BorrowInfo {
    let len = bc.scopes.len();
    bc.scopes[len - 1].locals.get_mut(ident).unwrap()
}

pub fn borrow_check_file<'a>(file: &'a File) -> u32 {
    let mut bc = BorrowContext {
        file,
        scopes: Vec::new(),
        temp_symbol: intern_string(".temp"),
        temp_index: 0,
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
    push_borrow_scope(bc);

    let num_stmts = block.stmts.len();
    let mut ret_info: Option<BorrowInfo> = None;
    for i in 0..num_stmts {
        ret_info = borrow_check_stmt(bc, &block.stmts[i]);
        if let Some(info) = ret_info {
            if info.from_return || i == (num_stmts - 1) {
                if let Some(owner_ident) = info.borrowed_from {
                    // NOTE(alexander): Cannot return something borrowed from this function, since it
                    // requires lifetime annotation or 'static lifetime which we don't support.
                    let err_msg = create_error_msg(
                        bc, ErrorLevel::Error, info.used_at.unwrap_or(info.declared_at),
                        "cannot return value borrowed from this function",
                        "borrowed value will outlive owned value");
                    print_error_msg(&err_msg);
                    bc.error_count += 1;
                }
            }
        }
    }

    pop_borrow_scope(bc);
}

fn borrow_check_stmt<'a>(bc: &mut BorrowContext<'a>, stmt: &'a Stmt) -> Option<BorrowInfo> {
    match stmt {
        Stmt::Local(local) => {
            if let Some(expr) = &*local.init {
                let borrow_info = borrow_check_expr(bc, expr);
                if let Some(info) = borrow_info {
                    let len = bc.scopes.len();
                    let ident = Ident {
                        symbol: local.ident.sym,
                        index: 0
                    };
                    bc.scopes[len - 1].locals.insert(ident, info);
                } else {
                    // TODO(alexander): probably better to use local.init span.
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
        Expr::Block(block) => {
            borrow_check_block(bc, &block.block);
            None
        }

        Expr::If(if_expr) => {
            borrow_check_expr(bc, &*if_expr.cond);
            borrow_check_block(bc, &if_expr.then_block);
            if let Some(block) = &if_expr.else_block {
                borrow_check_block(bc, block);
            }
            None
        }

        Expr::Ident(ident_expr) => {
            let len = bc.scopes.len();
            let ident = Ident {
                symbol: ident_expr.sym,
                index: 0,
            };
            let borrow_info = bc.scopes[len - 1].locals.get_mut(&ident).unwrap();
            if let None = borrow_info.used_at {
                borrow_info.used_at = Some(ident_expr.span);
            }
            Some(bc.scopes[len - 1].locals.get(&ident).unwrap().clone())
        }

        Expr::Lit(lit) => Some(insert_temp_borrow_info(bc, None, lit.span)),

        Expr::Return(ret) => {
            if let Some(expr) = &*ret.expr {
                let mut borrow_info = borrow_check_expr(bc, &expr);
                if let Some(info) = &mut borrow_info {
                    info.from_return = true;
                }
                borrow_info
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
                info.declared_at = expr.span;
                
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
                                     resolve_symbol(owner_ident.symbol)),
                            "immutable borrow occurs here");
                        print_error_msg(&err_msg);
                        bc.error_count += 1;
                        
                    } else if immutable_refs > 0 {
                        let err_msg = create_error_msg(
                            bc, ErrorLevel::Error, expr.span,
                            &format!("cannot borrow `{}` as mutable because it is also borrowed as immutable",
                                     resolve_symbol(owner_ident.symbol)),
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
                                     resolve_symbol(owner_ident.symbol)),
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
