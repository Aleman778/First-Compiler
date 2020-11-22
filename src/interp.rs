use std::fmt;
use std::collections::HashMap;
use crate::ast::*;
use crate::value::{Val, ValData};
use crate::intrinsics::*;
use crate::error::{ErrorLevel, ErrorMsg};

/**
 * Results used for the interpreter
 */
type IResult<T> = Result<T, ErrorMsg>;

/**
 * This represents the current state of the interpreter running
 * the program described in the file field.
 */
pub struct InterpContext<'a> {
    /// The current ast file being interpreted.
    file: &'a File,

    /// Stores item signatures, maps strings to items.
    signatures: HashMap<String, Item>,

    /// The call stack is a stack containing scopes.
    call_stack: Vec<Scope>,

    /// The stack storage, stores local variables.
    memory: Memory,

    /// Pointer to the base of the current stack frame stored in memory
    base_pointer: usize,
}

/**
 * Scope represents either some function scope, or sub block expression.
 * It is used to hold variables and memory addresses to locals to this scope.
 */
#[derive(Clone)]
pub struct Scope {
    /// The child scope, used for sub-block expressions.
    child: Box<Option<Scope>>,

    /// Variable memory mapper, maps strings to memory addresses.
    symbols: HashMap<String, usize>,

    /// Address to values stored in this scope, used to free memory later.
    values: Vec<usize>,

    /// The location in code where this scope was created from.
    pub span: Span,
}

/**
 * The main memory storage for interpreter.
 */
pub struct Memory {
    /// The data storage is a vector of memory entries.
    data: Vec<MemEntry>,

    /// The next address of unallocated space.
    next: usize,
}

/**
 * Memory entry is an entry in the memory storage.
 * An entry holds the data and some other meta data.
 */
#[derive(Clone)]
struct MemEntry {
    /// Is this memory entry mutable, or immuatable?
    mutable: bool,

    /// Is this entry reserved or available to be allocated.
    reserved: bool,

    /// The actual data that is stored in the memory entry.
    data: ValData,
}

/**
 * Implementation of the interpreter context.
 */
impl InterpContext<'a> {
    /**
     * Constructs a new that will execute the provided ast file.
     */
    pub fn new() -> Self {
        InterpContext {
            file: None,
            signatures: HashMap::new(),
            call_stack: Vec::new(),
            memory: Memory::new(),
        }
    }

    /**
     * Pushes a new block scope on the environment.
     * Note: there has to be a scope already on the call stack.
     */
    pub fn push_block(&mut self, new_scope: Scope) -> IResult<()> {
        let scope = self.current_scope()?;
        scope.push(new_scope);
        Ok(())
    }

    /**
     * Pops the latest pushed block scope. The variables
     * in this scope are freed, they go out of scope.
     */
    pub fn pop_block(&mut self) -> IResult<()> {
        let len = self.call_stack.len();
        let scope = &mut self.call_stack[len - 1];
        let popped = scope.pop(self)?;
        for addr in popped.addresses() {
            self.memory.free(self, addr)?;
        }
        Ok(())
    }

    /**
     * Push a function call scope on the call stack from the given id and with
     * specific argument values. The arguments are stored in the new scope.
     */
    pub fn push_function(&mut self, func: &FnItem, values: Vec<Val>) -> IResult<()> {
        let mut new_scope = Scope::new(func.span);
        let inputs = &func.decl.inputs;
        if inputs.len() == values.len() {
            for i in 0..inputs.len() {
                let arg_ty = &inputs[i].ty;
                let val_ty = &values[i].get_type();
                if arg_ty != val_ty {
                    let span = values[i].span;
                    return Err(self.mismatched_types_fatal_error(span, arg_ty, val_ty));
                }
                let id = &inputs[i].ident;
                let addr = self.memory.alloc(self, &values[i], inputs[i].mutable)?;
                new_scope.register(id, addr);
            }
            self.call_stack.push(new_scope);
            Ok(())
        } else {
            let mut err = self.fatal_error(
                format!("this function takes {} parameters but {} parameters were supplied",
                        inputs.len(),
                        values.len()),
                ""
            );
            let mut span = Span::from_bounds(
                func.span.start, func.block.span.start, func.span.loc
            );
            span.end.column += 1;
            err.span_label(span, "defined here");
            Err(err)
        }
    }

    /**
     * Pops and removes the the latest function call scope of the call stack
     */
    pub fn pop_function(&mut self) -> IResult<()> {
        match self.call_stack.pop() {
            Some(scope) => {
                for addr in scope.addresses() {
                    self.memory.free(addr)?;
                }
                Ok(())
            },
            None => Err(self.fatal_error("cannot pop an empty call stack", "")),
        }
    }

    /**
     * Stores an item signature from the given item.
     */
    pub fn store_item(&mut self, item: Item) {
        let key = item.get_id();
        if !self.signatures.contains_key(&key) {
            self.signatures.insert(key, item);
        }
    }

    /**
     * Loads an item from a given identifier.
     */
    pub fn load_item(&self, ident: &ExprIdent) -> IResult<Item> {
        match self.signatures.get(&ident.to_string) {
            Some(item) => Ok(item.clone()), // FIXME(alexander): this is presumably really inefficient!
            None => {
                self.fatal_error(
                    ident.span,
                    format!("cannot find function `{}` in this scope", ident.to_string),
                    ""
                );
                Err(())
            }
        }
    }

    /**
     * Stores a variable into memory and registers it in the current scope.
     * Note: if trying to store to previously allocated variable then
     * that value will be overwritten with the provided value (shadowing).
     */
    pub fn store_var(&mut self, ident: &ExprIdent, mutable: bool, val: &Val) -> IResult<()> {
        let len = self.call_stack.len();
        let scope = &mut self.call_stack[len - 1];
        let addr = self.memory.alloc(val, mutable)?;
        scope.register(&ident, addr);
        Ok(())
    }

    /**
     * Stores a value without an identifier and registes it in the current scope.
     * This method returns the address to this value so it can be accessed without identifier.
     */
    pub fn store_val(&mut self, val: &Val) -> IResult<usize> {
        let len = self.call_stack.len();
        let scope = &mut self.call_stack[len - 1];
        let addr = self.memory.alloc(val, false)?;
        scope.register_addr(addr);
        Ok(addr)
    }

    /**
     * Assigns an already allocated mutable variable and updates the memory.

     */
    pub fn assign_var(&mut self, addr: usize, val: &Val) -> IResult<()> {
        self.memory.store(addr, val)
    }

    /**
     * Loads an already allocated variable from the given identifier.
     */
    pub fn load_var(&mut self, ident: &ExprIdent) -> IResult<Val> {
        let len = self.call_stack.len();
        let scope = &mut self.call_stack[len - 1];
        let addr = scope.address_of(&ident, true)?;
        Ok(Val::from_data(self.memory.load(addr)?, Some(ident.to_string.clone()), ident.span))
    }

    /**
     * Loads a value directly from memory at the given memory address.
     * Note: reading outside allocated memory will result in memory error.
     */
    pub fn load_val(&mut self, addr: usize) -> IResult<ValData> {
        self.memory.load(addr)
    }

    /**
     * Looks up the address of a given identifier in the current scope.
     * Enabling backtracking allows you to find variables outside your scope.
     */
    pub fn address_of(&mut self, ident: &ExprIdent, backtrack: bool) -> IResult<usize> {
        let len = self.call_stack.len();
        let scope = &mut self.call_stack[len - 1];
        scope.address_of(&ident, backtrack)
    }

    /**
     * Returns a mutable reference to the highest scope in the call stack.
     */
    pub fn current_scope(&mut self) -> IResult<&mut Scope> {
        let len = self.call_stack.len();
        if len > 0 {
            Ok(&mut self.call_stack[len - 1])
        } else {
            Err(self.fatal_error("the call stack is empty"))
        }
    }

    /**
     * Formatting the list of signatures for printing the stack trace.
     */
    pub fn fmt_signatures(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write_str(f, "\nSignatures: [", indent)?;
        if self.signatures.len() > 8 {
            write_str(f, "\n", indent)?;
        }
        let mut result = String::new();
        for (sig, _) in &self.signatures {
            result.push_str(&format!("\"{}\", ", sig));
        }
        result.pop();
        result.pop();
        write_str(f, &result, indent)?;
        if self.signatures.len() > 8 {
            write_str(f, "\n", indent)?;
        }
        write_str(f, "]", indent)
    }

    /**
     * Formatting the call stack for printing the stack trace.
     */
    pub fn fmt_call_stack(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write_str(f, "\nCall Stack: {", indent)?;
        for (index, scope) in self.call_stack.iter().rev().enumerate() {
            scope.fmt_scope(f, self.file, indent + 8, index)?;
        }
        write_str(f, "\n}", indent)
    }

    /**
     * Creates a fatal error, convenience function.
     */
    pub fn fatal_error(&self, span: Span, message: &str, label: &str) -> ErrorMsg {
        ErrorMsg::from_span(
            ErrorLevel::Fatal,
            self.file.lines,
            span,
            &self.file.filename,
            &self.file.source,
            message,
            label
        );
    }

    pub fn mismatched_types_fatal_error(&self, span: Span, expected: TyKind, found: Ty) -> ErrorMsg {
        ErrorMsg::from_span(
            ErrorLevel::Fatal,
            self.file.lines,
            span,
            &self.file.filename,
            &self.file.source,
            format!("expected `{}`, found `{}`", expected, found),
            ""
        )
    }
}

/**
 * Helper function for writing strings with indentation.
 */
pub fn write_str(buf: &mut fmt::Formatter<'_>, mut s: &str, indent: usize) -> fmt::Result {
    while !s.is_empty() {
        let newline;
        let split = match s.find('\n') {
            Some(pos) => {
                newline = true;
                pos + 1
            },
            None => {
                newline = false;
                s.len()
            },
        };
        buf.write_str(&s[..split])?;
        s = &s[split..];

        if newline {
            buf.write_str(&" ".repeat(indent))?;
        }
    }

    Ok(())
}

/**
 * Formatting tracing of the context.
 */
impl<'a> fmt::Debug for InterpContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_str(f, "Trace:", 0)?;
        self.fmt_signatures(f, 4)?;
        self.fmt_call_stack(f, 4)?;
        write_str(f, &format!("\n{:#?}", self.memory), 4)
    }
}

/**
 * Implementation of the memory module.
 */
impl Memory {
    /**
     * Constructs an empty memory module. Default capacity
     * is 1000, use `Memory::with_capacity` to set a custom capacity.
     */
    pub fn new() -> Self {
        Memory {
            data: vec![MemEntry::new(); 1000],
            next: 0,
        }
    }

    /**
     * Constructs an empty memory module with specific capacity.
     */
    pub fn with_capacity(capacity: usize) -> Self {
        Memory {
            data: vec![MemEntry::new(); capacity],
            next: 0,
        }
    }

    /**
     * Allocates value data and returns the memory address to that data.
     */
    pub fn alloc(&mut self, ic: &mut InterpContext, val: &Val, mutable: bool) -> IResult<usize> {
        if self.next < self.data.capacity() {
            self.data[self.next] = MemEntry {
                mutable,
                reserved: true,
                data: val.data.clone(),
            };
            let addr = self.next;
            self.find_next();
            Ok(addr)
        } else {
            self.fatal_error(ic, val.span, "out of memory error");
            Err(())
        }
    }

    /**
     * Loads a value from memory at specific address.
     */
    pub fn load(&self, ic: &mut InterpContext, addr: usize) -> IResult<ValData> {
        if addr < self.data.capacity() {
            let entry = &self.data[addr];
            if entry.data.has_data() {
                Ok(entry.data.clone())
            } else {
                Err(self.fatal_error(ic, "reading unallocated memory"))
            }
        } else {
            Err(self.fatal_error(ic, "reading out of bounds"))
        }
    }

    /**
     * Stores a new value at a specific memory address.
     * Note: has to be an already allocated address.
     */
    pub fn store(&mut self, ic: &mut InterpContext, addr: usize, val: &Val) -> IResult<()> {
        if addr < self.data.capacity() {
            let entry = &mut self.data[addr];
            if entry.reserved || entry.data.has_data() {
                if entry.mutable || !entry.data.has_data() {
                    entry.data = val.data.clone();
                    Ok(())
                } else {
                    Err(self.fatal_error(ic, val.span, "cannot assign twice to immutable variable"))
                }
            } else {
                Err(self.fatal_error(ic, val.span, "cannot update unallocated memory"))
            }
        } else {
            Err(self.fatal_error(ic, "writing out of bounds"))
        }
    }

    /**
     * Frees the memory at the specific memory address.
     */
    pub fn free(&mut self, ic: &mut InterpContext, addr: usize) -> IResult<()> {
        if addr < self.data.capacity() {
            if self.is_alloc(addr) {
                let entry = &mut self.data[addr];
                entry.data = ValData::None;
                entry.reserved = false;
                if self.next > addr {
                    self.next = addr;
                }
                Ok(())
            } else {
                Err(self.fatal_error(ic, "cannot free unallocated memory"))
            }
        } else {
            Err(self.fatal_error(ic, "cannot free out of bounds"))
        }
    }

    /**
     * Find the next unallocated memory address.
     */
    fn find_next(&mut self) {
        loop {
            self.next += 1;
            if self.next < self.data.capacity() {
                if !self.is_alloc(self.next) {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /**
     * Check if a memory address is allocated, or reserved for future allcations.
     */
    fn is_alloc(&self, addr: usize) -> bool {
        let entry = &self.data[addr];
        entry.reserved || entry.data.has_data()
    }

    /**
     * Dumps the memory from address 0 to the last allocated address.
     */
    fn memory_dump(&self) -> String {
        let mut dump = String::new();
        dump.push_str("[");
        for i in 0..self.next {
            let mut mutable = String::new();
            if self.data[i].mutable {
                mutable.push_str(" mut");
            }
            dump.push_str(&format!("\n    [{}{}] = {:?},", i, mutable, self.data[i]));
        }
        dump.pop();
        dump.push_str("\n]");
        return dump;
    }
}

/**
 * Formatting the memory so it does print the entire memory state.
 */
impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Memory:")
            .field("capacity", &self.data.len())
            .field("next", &self.next)
            .field("data", &format_args!("{}", self.memory_dump()))
            .finish()
    }
}

/**
 * Implementation of memory entry.
 */
impl MemEntry {
    /**
     * Creates a new empty memory entry.
     */
    pub fn new() -> Self {
        MemEntry {
            mutable: false,
            reserved: false,
            data: ValData::None,
        }
    }
}

/**
 * Debug formatting for memory entry.
 */
impl fmt::Debug for MemEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.reserved || self.data.has_data() {
            write!(f, "{}", self.data)
        } else {
            write!(f, "reserved")
        }
    }
}

/**
 * Implementation of scope.
 */
impl Scope {
    /**
     * Constructs a new scope.
     */
    pub fn new(span: Span) -> Self {
        Scope {
            child: Box::new(None),
            symbols: HashMap::new(),
            values: vec![],
            span: span,
        }
    }

    /**
     * Returns the address of a given variable identifier.
     * If backtracking is enabled than this will also search child scopes.
     */
    pub fn address_of(&self, id: &ExprIdent, ic: &mut InterpContext, backtrack: bool) -> IResult<usize> {
        match &*self.child {
            Some(child) => {
                match child.address_of(id, ic, backtrack) {
                    Ok(addr) => Ok(addr),
                    Err(e) => {
                        if backtrack {
                            self.find_mem(id, ic)
                        } else {
                            Err(e)
                        }
                    }
                }
            },
            None => self.find_mem(id, ic),
        }
    }

    /**
     * Returns the registered addresses.
     */
    pub fn addresses(&self) -> Vec<usize> {
        let mut addresses = self.symbols.iter().map(|(_, addr)| *addr).collect::<Vec<usize>>();
        addresses.extend(&self.values);
        addresses
    }

    /**
     * Registers the given address and identifier in the variable list.
     * If you register an already existing identifier then the previous address
     * will be changed to a regular value that is only accessable via the address.
     * This enables the ability to shadow already existing let bindings.
     */
    pub fn register(&mut self, id: &ExprIdent, addr: usize) {
        match &mut *self.child {
            Some(child) => child.register(id, addr),
            None => {
                match self.symbols.insert(id.to_string.clone(), addr) {
                    Some(prev) => self.values.push(prev),
                    None => { },
                }
            }
        };
    }

    /**
     * Registers the given address so it can be freed when exiting this scope.
     */
    pub fn register_addr(&mut self, addr: usize) {
        match &mut *self.child {
            Some(child) => child.register_addr(addr),
            None => self.values.push(addr),
        }
    }

    /**
     * Push a new scope onto the outer most scope.
     */
    pub fn push(&mut self, new_scope: Scope) {
        match &mut *self.child {
            Some(child) => child.push(new_scope),
            None => self.child = Box::new(Some(new_scope)),
        }
    }

    /**
     * Pops the outer most scope. Returns the popped scope.
     */
    pub fn pop(&mut self, ic: &mut InterpContext) -> IResult<Scope> {
        let (opt, _) = self.pop_impl();
        match opt {
            Some(scope) => Ok(scope.clone()),
            None => Err(self.fatal_error(ic, "cannot pop the function scope")),
        }
    }

    /**
     * Pop implementation returns true if the outer most
     * scope has been reached and should be popped.
     */
    fn pop_impl(&mut self) -> (Option<Scope>, bool) {
        match &mut *self.child {
            Some(child) => {
                let (mut scope, found) = child.pop_impl();
                if found {
                    scope = Some(child.clone());
                    self.child = Box::new(None);
                }
                (scope, false)
            },
            None => (None, true),
        }
    }

    /**
     * Find an address in the memory using the provided identifier.
     * Returns memory error if not found in this scope.
     */
    fn find_mem(&self, ident: &ExprIdent, ic: &mut InterpContext) -> IResult<usize> {
        match self.symbols.get(&ident.to_string) {
            Some(addr) => Ok(*addr),
            None => Err(self.fatal_error(ic, ident.span, "not found in this scope")),
        }
    }

    /**
     * Debug formatting of this scope.
     */
    pub fn fmt_scope(
        &self,
        f: &mut fmt::Formatter<'_>,
        file: &File,
        mut indent: usize,
        index: usize
    ) -> fmt::Result {
        write_str(f, &format!("\n{}: ", index), indent - 4)?;
        self.fmt_sub_scope(f, indent + 4, &file, index, 1)?;
        if (*self.child).is_some() {
            write_str(f, &format!("\n{}.0: ", index), indent)?;
            indent += 4;
        }
        let line = self.span.start.line;
        if line > 0 {
            write_str(f, file.get_line(line).trim(), indent)?;
            write_str(f, &format!("\n at {}:{}", file.filename, line), indent)?;
        } else {
            write_str(f, &format!("{}", file.filename), indent)?;
        }
        if self.symbols.len() > 0 {
            write_str(f, &format!("\nSymbols Table: {:#?},", &self.symbols), indent)?;
        }
        write_str(f, "\n},", indent - 4)
    }

    /**
     * Debug formatting of this child scope.
     */
    pub fn fmt_sub_scope(
        &self,
        f: &mut fmt::Formatter<'_>,
        indent: usize,
        file: &File,
        index: usize,
        sub_index: usize
    ) -> fmt::Result {
        match &*self.child {
            Some(child) => {
                write_str(f, &format!("\n{}.{}: ", index, sub_index), indent - 4)?;
                let line = child.span.start.line;
                if line > 0 {
                    let code = file.get_line(line).trim().to_owned();
                    if code == "{" {
                        write_str(f, "<block>", indent)?;
                    } else {
                        write_str(f, &code.trim(), indent)?;
                    }
                    write_str(f, file.get_line(line).trim(), indent)?;
                    write_str(f, &format!("\n at {}:{}", file.name, line), indent)?;
                } else {
                    write_str(f, &format!("{}", file.name), indent)?;
                }
                if self.symbols.len() > 0 {
                    write_str(f, &format!("\nSymbols Table: {:#?},", &child.symbols), indent)?;
                }
                child.fmt_sub_scope(f, indent + 4, file, index, sub_index + 1)?;
                write_str(f, "\n},", indent - 4)
            }
            None => Ok(()),
        }
    }
}

/**
 * Evaluates a source file and executes the main method.
 */
pub fn interp_file<'a>(ic: &mut InterpContext<'a>, file: &'a File) {
    ic.file = Some(file);
    for item in &file.items {
        interp_item(ic, &item);
    }
}

/**
 * Interpret an item, this does not call the function items, use
 * interp call to actually execute the fuction.
 */
pub fn interp_item<'a>(ic: &mut InterpContext<'a>, item: &Item) {
    match item {
        Item::Fn(_) => ic.store_item(item.clone()),
        Item::ForeignMod(module) => {
            for foreign_item in &module.items {
                interp_item(ic, foreign_item);
            }
        }
        _ => { },
    }
}

/**
 * Interpret the entry point, i.e. the `main` function is executed.
 */
pub fn interp_entry_point<'a>(ic: &mut InterpContext<'a>) -> IResult<i32> {
    let main_function = match ic.signatures.get("main") {
        Some(item) => {
            match item {
                Item::Fn(func) => func,
                _ => return Err(ic.fatal_error("main exists but is not a valid function", "")),
            }
        }
        None => return Err(ic.fatal_error("there is no main function", "")),
    };

    ic.push_function(&main_function, vec![])?;
    let val = interp_block(ic, main_function.block)?;
    ic.pop_function()?;
    match val.data {
        ValData::Continue |
        ValData::Break => Err(ic.fatal_error(ic.ic, val.span, "cannot break outside loop")),
        _ => Ok(val),
    }

}

// impl Item {
//     pub fn eval_func<'a>(&self, values: Vec<Val>, ic: &mut InterpContext) -> IResult<Val> {
//         match self {
//             Item::Fn(func) => func.eval(values, ic),
//             Item::ForeignFn(func) => func.eval(values, ic),
//             _ => {
//                 let ident = self.get_ident();
//                 let mut err = self.fatal_error(
//                     ic.ic,
//                     ident.span,
//                     "cannot find function `{}` in this scope",
//                     ident.to_string
//                 );
//                 err.span_label(ident.span, "not found in this scope");
//                 Err(err)
//             },
//         }
//     }
// }

/**
 * Evaluates a function item.
 */
// impl FnItem {
    // fn eval<'a>(&self, values: Vec<Val>, ic: &mut InterpContext) -> IResult<Val> {
        // ic.push_function(&self, values)?;
        // let val = self.block.eval(ic)?;
        // ic.pop_function()?;
        // match val.data {
            // ValData::Continue |
            // ValData::Break => Err(ic.fatal_error(ic.ic, val.span, "cannot break outside loop")),
            // _ => Ok(val),
        // }
    // }
// }

/**
 * Interpret foreign function call given array of argument values.
 */
fn interp_intrinsics<'a>(ic: &mut InterpContext<'a>, item: &ForeignFnItem, values: Vec<Val>) -> IResult<Val> {
    match item.ident.to_string.as_str() {
        "trace" => {
            trace(ic);
        },
        "print_int" => {
            match values[0].get_i32() {
                Some(arg) => print_int(arg),
                None => return Err(ic.mismatched_types_fatal_error(
                    ic.ic, values[0].span, TyKind::Int(IntTy::I32), values[0].get_type()))
            };
        },
        "print_bool" => {
            match values[0].get_bool() {
                Some(arg) => print_bool(arg),
                None => return Err(ic.mismatched_types_fatal_error(
                    ic.ic, values[0].span, TyKind::Bool, values[0].get_type())),
            };
        },
        "assert" => {
            match values[0].get_bool() {
                Some(arg) => assert(arg),
                None => return Err(ic.mismatched_types_self.fatal_error(
                    ic.ic, values[0].span, TyKind::Bool, values[0].get_type())),
            };
        },
        "assert_eq_int" => {
            match values[0].get_i32() {
                Some(arg0) => match values[1].get_i32() {
                    Some(arg1) => assert_eq_int(arg0, arg1),
                    None => return Err(ic.mismatched_types_fatal_error(
                        ic.ic, values[1].span, TyKind::Int(IntTy::I32), values[1].get_type()))
                }
                None => return Err(ic.mismatched_types_self.fatal_error(
                    ic.ic, values[0].span, TyKind::Int(IntTy::I32), values[0].get_type()))
            };
        },
        "assert_eq_bool" => {
            match values[0].get_bool() {
                Some(arg0) => match values[1].get_bool() {
                    Some(arg1) => assert_eq_bool(arg0, arg1),
                    None => return Err(ic.mismatched_types_fatal_error(
                        ic.ic, values[1].span, TyKind::Bool, values[1].get_type()))
                }
                None => return Err(ic.mismatched_types_fatal_error(
                    ic.ic, values[0].span, TyKind::Bool, values[0].get_type()))
            };
        },
        _ => return Err(ic.fatal_error(Span::new(), format!("`{}` is not a valid intrinsic function",
                                                            item.ident.to_string()))),
    };
    Ok(Val::new())
}

/**
 * Interprets the block expression with the given interp context
 * and pushes the new block if the given new_scope is true.
 */
fn interp_block<'a>(ic: &mut InterpContext<'a>, block: &Block) -> IResult<Val> {
    let mut ret_val = Val::new();
    for stmt in &block.stmts {
        let val = interp_stmt(ic, stmt)?;
        match val.data {
            ValData::None => continue,
            _ => ret_val = val,
        };
        break;
    }
    Ok(ret_val)
}

/**
 * Evaluates an statement
 */
fn interp_stmt<'a>(ic: &mut InterpContext<'a>, stmt: &Stmt) -> IResult<Val> {
    match stmt {
        Stmt::Local(local) => {
            let val = match &*local.init {
                Some(init) => {
                    let val = init.eval(ic)?;
                    let val_ty = val.get_type();
                    if val_ty != local.ty {
                        return Err(ic.mismatched_types_fatal_error(ic.ic, val.span, local.ty, val_ty));
                    }
                    val
                }
                None => Val::new(),
            };
            ic.store_var(&local.ident, local.mutable, &val)?;
            Ok(Val::new())
        }
        
        Stmt::Semi(expr) => {
            let ret_val = expr.eval(ic)?;
            // Only perform explicit returns e.g. return expression.
            Ok(match expr {
                Expr::Return(_) => ret_val,
                Expr::Continue(_) => ret_val,
                Expr::Break(_) => ret_val,
                _ => Val::new(),
            })
        },
        
        Stmt::Expr(expr) => interp_expr(ic),
        
        Stmt::Item(_item) => Err(ic.fatal_error(stmt.span, "items in functions are not supported by the interpreter")),
    }
}

/**
 * Evaluates a local variable assignment.
 */
fn interp_local_stmt<'a>(ic: &mut InterpContext<'a>, local: &Local) -> IResult<Val> {
    
}

/**
 * Implementation of binary operator for evaluating binary expressions.
 */
pub fn interp_binary_expr<'a>(
    ic: &mut InterpContext<'a>,
    op: BinOp,
    left: Val,
    right: Val,
    combined_span: Span
) -> IResult<Val> {
    
    let left_type = left.get_type();
    let right_type = right.get_type();
    let (span, result) = match op {
        BinOp::Add{span} => (span, left.add  (right, combined_span)),
        BinOp::Sub{span} => (span, left.sub  (right, combined_span)),
        BinOp::Div{span} => (span, left.div  (right, combined_span)),
        BinOp::Mul{span} => (span, left.mul  (right, combined_span)),
        BinOp::Pow{span} => (span, left.pow  (right, combined_span)),
        BinOp::Mod{span} => (span, left.r#mod(right, combined_span)),
        BinOp::And{span} => (span, left.and  (right, combined_span)),
        BinOp::Or {span} => (span, left.or   (right, combined_span)),
        BinOp::Eq {span} => (span, left.eq   (right, combined_span)),
        BinOp::Ne {span} => (span, left.ne   (right, combined_span)),
        BinOp::Lt {span} => (span, left.lt   (right, combined_span)),
        BinOp::Le {span} => (span, left.le   (right, combined_span)),
        BinOp::Gt {span} => (span, left.gt   (right, combined_span)),
        BinOp::Ge {span} => (span, left.ge   (right, combined_span)),
    };

    match result {
        Some(val) => Ok(val),
        None => {
            Err(ic.fatal_error(
                *span,
                &format!("cannot {} `{}` to `{}`", op, left_type, right_type),
                &format!("no implementation for `{} {} {}`", left_type, op.token(), right_type)))
        },
    }
}

/**
 * Implementation of unary oprator for evaluating unary expressions.
 */
pub fn interp_unary_expr<'a>(ic: &mut InterpContext, val: Val, combined_span: Span) -> IResult<Val> {
    let val_type = val.get_type();
    let (span, result) = match self {
        UnOp::Neg{span}   => (span, val.neg(combined_span)),
        UnOp::Not{span}   => (span, val.not(combined_span)),
        UnOp::Deref{span} => (span, val.deref(combined_span, ic)?),
    };

    match result {
        Some(val) => Ok(val),
        None => {
            let mut err = self.fatal_error(
                ic.ic,
                *span,
                "type `{}` cannot be {}",
                val_type,
                self,
            );
            err.span_label(
                *span,
                &format!("no implementation for `{}{}`", self.token(), val_type)
            );
            Err(err)
        }
    }
}

/**
 * Evaluate an expression.
 */

    fn interp_expr(ic: &mut InterpContext, expr: &Expr) -> IResult<Val> {
        match expr {
            Expr::Assign    (e) => e.eval(ic),
            Expr::Binary    (e) => e.eval(ic),
            Expr::Block     (e) => e.eval(ic),
            Expr::Break     (e) => e.eval(ic),
            Expr::Call      (e) => e.eval(ic),
            Expr::Continue  (e) => e.eval(ic),
            Expr::Ident     (e) => e.eval(ic),
            Expr::If        (e) => e.eval(ic),
            Expr::Lit       (e) => e.eval(ic),
            Expr::Paren     (e) => e.eval(ic),
            Expr::Reference (e) => e.eval(ic),
            Expr::Return    (e) => e.eval(ic),
            Expr::Unary     (e) => e.eval(ic),
            Expr::While     (e) => e.eval(ic),
        }
    }

    /**
     * Evaluates the memory address of a given expression.
     */
    fn interp_addr_of_expr(&self, ic: &mut InterpContext) -> IResult<usize> {
        match self {
            Expr::Ident(ident) => {
                ic.address_of(&ident, true)
            },
            Expr::Unary(unary) => {
                match unary.op {
                    UnOp::Deref{span: _} => {
                        let addr = (*unary.right).eval_addr(ic)?;
                        match ic.load_val(addr)? {
                            ValData::Ref(r) => {
                                if r.mutable {
                                    Ok(r.addr)
                                } else {
                                    let mut err = self.fatal_error(
                                        ic.ic,
                                        self.get_span(),
                                        "cannot assign to variable using immutable reference");
                                    err.span_label(r.ref_ty.span, "help: consider changing this to be a mutable reference");
                                    err.span_label(self.get_span(), "variable is a reference, so data it refers to cannot be written");
                                    Err(err)
                                }
                            }
                            _ => Err(self.fatal_error(ic.ic, "invalid expression")),
                        }
                    },
                    _ => Err(self.fatal_error(ic.ic, "invalid expression")),
                }
            },
            _ => Err(self.fatal_error(ic.ic, "invalid expression")),
        }
    }
}

/**
 * Evaluates an assignment expresson
 */
impl Eval for ExprAssign {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        let addr = match (*self.left).eval_addr(ic) {
            Ok(addr) => addr,
            Err(mut err) => {
                if err.message[0].text == "invalid expression" {
                    err.message[0].text = String::from("invalid left-hand side expression");
                    err.primary_span(self.span);
                    err.span_label(self.span, "left-hand of expression not valid");
                }
                return Err(err);
            }
        };
        let val = (*self.right).eval(ic)?;
        ic.assign_var(addr, &val)?;
        Ok(Val::new())
    }
}

/**
 * Evaluates a binary expression.
 */
impl Eval for ExprBinary {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        let left = self.left.eval(ic)?;
        let right = self.right.eval(ic)?;
        self.op.eval(ic, left, right, self.span)
    }
}

/**
 * Evaluates a block expression.
 */
impl Eval for ExprBlock {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        ic.push_block(Scope::new(self.span))?;
        let ret_val = self.block.eval(ic)?;
        ic.pop_block()?;
        Ok(ret_val)
    }
}

/**
 * Evaluates break expression.
 */
impl Eval for ExprBreak {
    fn eval(&self, _ic: &mut InterpContext) -> IResult<Val> {
        Ok(Val::from_data(ValData::Break, None, self.span))
    }
}

/**
 * Evaluates a function call.
 */
impl Eval for ExprCall {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        let mut values = Vec::new();
        for arg in &self.args {
            let val = arg.eval(ic)?;
            values.push(val);
        }
        let item = ic.load_item(&self.ident)?;
        match item.eval_func(values, ic) {
            Ok(val) => Ok(val),
            Err(mut err) => {
                let len = match item {
                    Item::Fn(func) => func.decl.inputs.len(),
                    Item::ForeignFn(func) => func.decl.inputs.len(),
                    _ => 0,
                };
                if self.args.len() != len {
                    err.primary_span(self.span);
                    err.span_label(self.span, &format!("expected {} parameters", len));
                }
                Err(err)
            },
        }
    }
}

/**
 * Evaluates continue expression.
 */
impl Eval for ExprContinue {
    fn eval(&self, _ic: &mut InterpContext) -> IResult<Val> {
        Ok(Val::from_data(ValData::Continue, None, self.span))
    }
}

/**
 * Evaluates to value by loading from memory using this identifier.
 */
impl Eval for ExprIdent {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        ic.load_var(&self)
    }
}

/**
 * Evaluates an if statement.
 */
impl Eval for ExprIf {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        let value = (*self.cond).eval(ic)?;
        match value.get_bool() {
            Some(cond) => {
                if cond {
                    ic.push_block(Scope::new(self.span))?;
                    let result = self.then_block.eval(ic);
                    ic.pop_block()?;
                    result
                } else {
                    match self.else_block.clone() {
                        Some(block) => {
                            ic.push_block(Scope::new(self.span))?;
                            let result = block.eval(ic);
                            ic.pop_block()?;
                            result
                        },
                        None => Ok(Val::new()),
                    }
                }
            },
            None => Err(ic.mismatched_types_fatal_error(ic.ic, value.span, TyKind::Bool, value.get_type())),
        }
    }
}

/**
 * Evaluates a literal expression.
 */
impl Eval for ExprLit {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        self.lit.eval(ic)
    }
}

/**
 * Evaluates a parenthesized expression.
 */
impl Eval for ExprParen {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        (*self.expr).eval(ic)
    }
}

/**
 * Evaluates a reference expression.
 */
impl Eval for ExprReference {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        let val = (*self.expr).eval(ic)?;
        if !val.has_data() {
            Ok(val)
        } else {
            let ref_ty = val.get_type();
            match val.ident {
                Some(name) => {
                    // Reference to an already existing variable.
                    let ident = ExprIdent {
                        to_string: name,
                        span: val.span,
                    };
                    let addr = ic.address_of(&ident, true)?;
                    let new_val = Val::from_ref(addr, ref_ty, self.mutable, self.span);
                    Ok(new_val)
                },
                None => {
                    // Reference to a new variable without identifier.
                    let addr = ic.store_val(&val)?;
                    let new_val = Val::from_ref(addr, ref_ty, self.mutable, self.span);
                    Ok(new_val)
                },
            }
        }
    }
}

/**
 * Evaluates a return statement.
 */
impl Eval for ExprReturn {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        match &*self.expr {
            Some(expr) => expr.eval(ic),
            None => Ok(Val::from_data(ValData::Void, None, self.span)),
        }
    }
}

/**
 * Evaluates a unary expression.
 */
impl Eval for ExprUnary {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        let right = self.right.eval(ic)?;
        self.op.eval(ic, right, self.span)
    }
}

/**
 * Evaluates a while loop.
 */
impl Eval for ExprWhile {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        loop {
            let value = (*self.cond).eval(ic)?;
            match value.get_bool() {
                Some(cond) => {
                    if cond {
                        ic.push_block(Scope::new(self.span))?;
                        let val = self.block.eval(ic)?;
                        ic.pop_block()?;
                        match val.data {
                            ValData::Continue => continue,
                            ValData::Break => break,
                            ValData::None => continue,
                            _ => return Ok(val),
                        };
                    } else {
                        break;
                    }
                },
                None => {
                    return Err(ic.mismatched_types_fatal_error(ic.ic, value.span, TyKind::Bool, value.get_type()));
                },
            };
        }
        Ok(Val::new())
    }
}

/**
 * Evaluates a literal.
 */
impl Eval for Lit {
    fn eval(&self, ic: &mut InterpContext) -> IResult<Val> {
        match self {
            Lit::Int(literal) => literal.eval(ic),
            Lit::Bool(literal) => literal.eval(ic),
            Lit::Str(literal)  => Err(self.fatal_error(
                ic, literal.span, "unsupported literal type by the interpreter")),
        }
    }
}

/**
 * Evaluates an int literal.
 */
impl Eval for LitInt {
    fn eval(&self, _ic: &mut InterpContext) -> IResult<Val> {
        Ok(Val::from_i32(self.value, self.span))
    }
}

/**
 * Evaluates a boolean literal.
 */
impl Eval for LitBool {
    fn eval(&self, _ic: &mut InterpContext) -> IResult<Val> {
        Ok(Val::from_bool(self.value, self.span))
    }
}
