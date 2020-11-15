use std::fmt;
use std::collections::HashMap;
use crate::ast::*;
use crate::value::{Val, ValData};
use crate::intrinsics::*;

/**
 * Results used for the interpreter
 */
pub type IResult<T> = Result<T, ()>;

/**
 * Eval trait is used to define the evaluation
 * function for each AST node.
 */
pub trait Eval {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val>;
}

/**
 * The environment used when interpreting a program.
 * Environment stores runtime information such as
 * the memory, call stack and item signatures etc.
 */
pub struct RuntimeEnv {
    /// The current ast file being interpreted.
    file: File,

    /// Stores item signatures, maps strings to items.
    signatures: HashMap<String, Item>,

    /// The call stack is a stack containing scopes.
    call_stack: Vec<Scope>,

    /// The stack storage, stores local variables.
    memory: Memory,
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
 * The scope is used to handle the memory inside a particular scope.
 * For nested blocks this scope acts as an item in a linked list.
 * The outer block is always defined inside the call stack for
 * the environment.
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
 * Implementation of the runtime environment.
 */
impl RuntimeEnv {
    /**
     * Constructs an empty environment.
     */
    pub fn new() -> Self {
        RuntimeEnv {
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
    pub fn push_func(&mut self, func: &FnItem, values: Vec<Val>) -> IResult<()> {
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
                "this function takes {} parameters but {} parameters were supplied",
                inputs.len(),
                values.len()
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
     * Push the main function scope on the call stack.
     */
    pub fn load_main(&mut self) -> IResult<FnItem> {
        match self.signatures.get("main") {
            Some(item) => {
                match item {
                    Item::Fn(func) => {
                        Ok(func.clone())
                    },
                    _ => Err(self.fatal_error("main exists but is not a valid function")),
                }
            }
            None => Err(self.fatal_error("there is no main function")),
        }
    }

    /**
     * Pops and removes the the latest function call scope of the call stack
     */
    pub fn pop_func(&mut self) -> IResult<()> {
        match self.call_stack.pop() {
            Some(scope) => {
                for addr in scope.addresses() {
                    self.memory.free(addr)?;
                }
                Ok(())
            },
            None => Err(self.fatal_error("cannot pop an empty call stack")),
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
                let mut err = self.fatal_error(
                    ident.span,
                    format!("cannot find function `{}` in this scope", ident.to_string)
                );
                err.span_label(ident.span, "not found in this scope");
                Err(err)
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

    pub fn fatal_error(&self, span: Span, ) {

    }

    pub fn mismatched_types_fatal_error() {

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
 * Formatting tracing of the runtime environment.
 */
impl<'a> fmt::Debug for RuntimeEnv {
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
    pub fn alloc(&mut self, env: &mut RuntimeEnv, val: &Val, mutable: bool) -> IResult<usize> {
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
            self.fatal_error(env, val.span, "out of memory error");
            Err(())
        }
    }

    /**
     * Loads a value from memory at specific address.
     */
    pub fn load(&self, env: &mut RuntimeEnv, addr: usize) -> IResult<ValData> {
        if addr < self.data.capacity() {
            let entry = &self.data[addr];
            if entry.data.has_data() {
                Ok(entry.data.clone())
            } else {
                Err(self.fatal_error(env, "reading unallocated memory"))
            }
        } else {
            Err(self.fatal_error(env, "reading out of bounds"))
        }
    }

    /**
     * Stores a new value at a specific memory address.
     * Note: has to be an already allocated address.
     */
    pub fn store(&mut self, env: &mut RuntimeEnv, addr: usize, val: &Val) -> IResult<()> {
        if addr < self.data.capacity() {
            let entry = &mut self.data[addr];
            if entry.reserved || entry.data.has_data() {
                if entry.mutable || !entry.data.has_data() {
                    entry.data = val.data.clone();
                    Ok(())
                } else {
                    Err(self.fatal_error(env, val.span, "cannot assign twice to immutable variable"))
                }
            } else {
                Err(self.fatal_error(env, val.span, "cannot update unallocated memory"))
            }
        } else {
            Err(self.fatal_error(env, "writing out of bounds"))
        }
    }

    /**
     * Frees the memory at the specific memory address.
     */
    pub fn free(&mut self, env: &mut RuntimeEnv, addr: usize) -> IResult<()> {
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
                Err(self.fatal_error(env, "cannot free unallocated memory"))
            }
        } else {
            Err(self.fatal_error(env, "cannot free out of bounds"))
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
    pub fn address_of(&self, id: &ExprIdent, env: &mut RuntimeEnv, backtrack: bool) -> IResult<usize> {
        match &*self.child {
            Some(child) => {
                match child.address_of(id, env, backtrack) {
                    Ok(addr) => Ok(addr),
                    Err(e) => {
                        if backtrack {
                            self.find_mem(id, env)
                        } else {
                            Err(e)
                        }
                    }
                }
            },
            None => self.find_mem(id, env),
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
    pub fn pop(&mut self, env: &mut RuntimeEnv) -> IResult<Scope> {
        let (opt, _) = self.pop_impl();
        match opt {
            Some(scope) => Ok(scope.clone()),
            None => Err(self.fatal_error(env, "cannot pop the function scope")),
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
    fn find_mem(&self, ident: &ExprIdent, env: &mut RuntimeEnv) -> IResult<usize> {
        match self.symbols.get(&ident.to_string) {
            Some(addr) => Ok(*addr),
            None => Err(self.fatal_error(env, ident.span, "not found in this scope")),
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
impl File {
    pub fn eval<'a>(&self, env: &mut RuntimeEnv) {
        for item in &self.items {
            match item {
                Item::Fn(_) => env.store_item(item.clone()),
                Item::ForeignMod(module) => module.eval(env),
                _ => { },
            }
        }
        match env.load_main() {
            Ok(func) => {
                if let Err(diagnostic) = func.eval(Vec::new(), env) {
                    env.env.emit(&diagnostic);
                    return;
                }
            },
            Err(diagnostic) => env.env.emit(&diagnostic),
        };
    }
}

/**
 * Implementation of item.
 */
impl Item {
    /**
     * Evaluates a function item.
     */
    pub fn eval_func<'a>(&self, values: Vec<Val>, env: &mut RuntimeEnv) -> IResult<Val> {
        match self {
            Item::Fn(func) => func.eval(values, env),
            Item::ForeignFn(func) => func.eval(values, env),
            _ => {
                let ident = self.get_ident();
                let mut err = self.fatal_error(
                    env.env,
                    ident.span,
                    "cannot find function `{}` in this scope",
                    ident.to_string
                );
                err.span_label(ident.span, "not found in this scope");
                Err(err)
            },
        }
    }
}

/**
 * Evaluates a function item.
 */

impl FnItem {
    fn eval<'a>(&self, values: Vec<Val>, env: &mut RuntimeEnv) -> IResult<Val> {
        env.push_func(&self, values)?;
        let val = self.block.eval(env)?;
        env.pop_func()?;
        match val.data {
            ValData::Continue |
            ValData::Break => Err(self.fatal_error(env.env, val.span, "cannot break outside loop")),
            _ => Ok(val),
        }
    }
}

/**
 * Evaluates a foreign function item.
 */
impl ForeignFnItem {
    fn eval(&self, values: Vec<Val>, env: &mut RuntimeEnv) -> IResult<Val> {
        match self.ident.to_string.as_str() {
            "trace" => {
                trace(env);
            },
            "print_int" => {
                match values[0].get_i32() {
                    Some(arg) => print_int(arg),
                    None => return Err(mismatched_types_fatal_error(
                        env.env, values[0].span, TyKind::Int(IntTy::I32), values[0].get_type()))
                };
            },
            "print_bool" => {
                match values[0].get_bool() {
                    Some(arg) => print_bool(arg),
                    None => return Err(mismatched_types_fatal_error(
                        env.env, values[0].span, TyKind::Bool, values[0].get_type())),
                };
            },
            "assert" => {
                match values[0].get_bool() {
                    Some(arg) => assert(arg),
                    None => return Err(mismatched_types_self.fatal_error(
                        env.env, values[0].span, TyKind::Bool, values[0].get_type())),
                };
            },
            "assert_eq_int" => {
                match values[0].get_i32() {
                    Some(arg0) => match values[1].get_i32() {
                        Some(arg1) => assert_eq_int(arg0, arg1),
                        None => return Err(mismatched_types_fatal_error(
                            env.env, values[1].span, TyKind::Int(IntTy::I32), values[1].get_type()))
                    }
                    None => return Err(mismatched_types_self.fatal_error(
                        env.env, values[0].span, TyKind::Int(IntTy::I32), values[0].get_type()))
                };
            },
            "assert_eq_bool" => {
                match values[0].get_bool() {
                    Some(arg0) => match values[1].get_bool() {
                        Some(arg1) => assert_eq_bool(arg0, arg1),
                        None => return Err(mismatched_types_fatal_error(
                            env.env, values[1].span, TyKind::Bool, values[1].get_type()))
                    }
                    None => return Err(mismatched_types_fatal_error(
                        env.env, values[0].span, TyKind::Bool, values[0].get_type()))
                };
            },
            _ => return Err(self.fatal_error(env.env, "is not a debug function")),
        };
        Ok(Val::new())
    }
}

impl ForeignModItem {
    fn eval(&self, env: &mut RuntimeEnv) {
        for item in &self.items {
            env.store_item(Item::ForeignFn(item.clone()));
        }
    }
}

/**
 * Evaluates the block expression with the given runtime env
 * and pushes the new block if the given new_scope is true.
 */
impl Eval for Block {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let mut ret_val = Val::new();
        for stmt in &self.stmts {
            let val = stmt.eval(env)?;
            match val.data {
                ValData::None => continue,
                _ => ret_val = val,
            };
            break;
        }
        Ok(ret_val)
    }
}

/**
 * Evaluates an statement
 */
impl Eval for Stmt {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match self {
            Stmt::Local(local) => local.eval(env),
            Stmt::Item(_item) => Err(self.fatal_error(env.env, "items in functions are not supported by the interpreter")),
            Stmt::Semi(expr) => {
                let ret_val = expr.eval(env)?;
                // Only perform explicit returns e.g. return expression.
                Ok(match expr {
                    Expr::Return(_) => ret_val,
                    Expr::Continue(_) => ret_val,
                    Expr::Break(_) => ret_val,
                    _ => Val::new(),
                })
            },
            Stmt::Expr(expr) => expr.eval(env),
        }
    }
}

/**
 * Evaluates a local variable assignment.
 */
impl Eval for Local {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let val = match &*self.init {
            Some(init) => {
                let val = init.eval(env)?;
                let val_ty = val.get_type();
                if val_ty != self.ty {
                    return Err(mismatched_types_fatal_error(env.env, val.span, self.ty, val_ty));
                }
                val
            }
            None => Val::new(),
        };
        env.store_var(&self.ident, self.mutable, &val)?;
        Ok(Val::new())
    }
}

/**
 * Implementation of binary operator for evaluating binary expressions.
 */
impl BinOp {
    pub fn eval<'a>(&self, env: &mut RuntimeEnv, left: Val, right: Val, combined_span: Span) -> IResult<Val> {
        let left_type = left.get_type();
        let right_type = right.get_type();
        let (span, result) = match self {
            BinOp::Add{span} => (span, left.add(right, combined_span)),
            BinOp::Sub{span} => (span, left.sub(right, combined_span)),
            BinOp::Div{span} => (span, left.div(right, combined_span)),
            BinOp::Mul{span} => (span, left.mul(right, combined_span)),
            BinOp::Pow{span} => (span, left.pow(right, combined_span)),
            BinOp::Mod{span} => (span, left.r#mod(right, combined_span)),
            BinOp::And{span} => (span, left.and(right, combined_span)),
            BinOp::Or{span}  => (span, left.or(right, combined_span)),
            BinOp::Eq{span}  => (span, left.eq(right, combined_span)),
            BinOp::Ne{span}  => (span, left.ne(right, combined_span)),
            BinOp::Lt{span}  => (span, left.lt(right, combined_span)),
            BinOp::Le{span}  => (span, left.le(right, combined_span)),
            BinOp::Gt{span}  => (span, left.gt(right, combined_span)),
            BinOp::Ge{span}  => (span, left.ge(right, combined_span)),
        };

        match result {
            Some(val) => Ok(val),
            None => {
                let mut err = self.fatal_error(
                    env.env,
                    *span,
                    "cannot {} `{}` to `{}`",
                    self,
                    left_type,
                    right_type
                );
                err.span_label(
                    *span,
                    &format!("no implementation for `{} {} {}`", left_type, self.token(), right_type)
                );
                Err(err)
            },
        }
    }
}

/**
 * Implementation of unary oprator for evaluating unary expressions.
 */
impl UnOp {
    pub fn eval<'a>(&self, env: &mut RuntimeEnv, right: Val, combined_span: Span) -> IResult<Val> {
        let right_type = right.get_type();
        let (span, result) = match self {
            UnOp::Neg{span}   => (span, right.neg(combined_span)),
            UnOp::Not{span}   => (span, right.not(combined_span)),
            UnOp::Deref{span} => (span, right.deref(combined_span, env)?),
        };

        match result {
            Some(val) => Ok(val),
            None => {
                let mut err = self.fatal_error(
                    env.env,
                    *span,
                    "type `{}` cannot be {}",
                    right_type,
                    self,
                );
                err.span_label(
                    *span,
                    &format!("no implementation for `{}{}`", self.token(), right_type)
                );
                Err(err)
            }
        }
    }
}

/**
 * Evaluate an expression.
 */
impl Eval for Expr {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match self {
            Expr::Assign(expr)    => expr.eval(env),
            Expr::Binary(expr)    => expr.eval(env),
            Expr::Block(expr)     => expr.eval(env),
            Expr::Break(expr)     => expr.eval(env),
            Expr::Call(expr)      => expr.eval(env),
            Expr::Continue(expr)  => expr.eval(env),
            Expr::Ident(expr)     => expr.eval(env),
            Expr::If(expr)        => expr.eval(env),
            Expr::Lit(expr)       => expr.eval(env),
            Expr::Paren(expr)     => expr.eval(env),
            Expr::Reference(expr) => expr.eval(env),
            Expr::Return(expr)    => expr.eval(env),
            Expr::Unary(expr)     => expr.eval(env),
            Expr::While(expr)     => expr.eval(env),
        }
    }
}

impl Expr {
    /**
     * Evaluates the memory address of a given expression.
     */
    fn eval_addr(&self, env: &mut RuntimeEnv) -> IResult<usize> {
        match self {
            Expr::Ident(ident) => {
                env.address_of(&ident, true)
            },
            Expr::Unary(unary) => {
                match unary.op {
                    UnOp::Deref{span: _} => {
                        let addr = (*unary.right).eval_addr(env)?;
                        match env.load_val(addr)? {
                            ValData::Ref(r) => {
                                if r.mutable {
                                    Ok(r.addr)
                                } else {
                                    let mut err = self.fatal_error(
                                        env.env,
                                        self.get_span(),
                                        "cannot assign to variable using immutable reference");
                                    err.span_label(r.ref_ty.span, "help: consider changing this to be a mutable reference");
                                    err.span_label(self.get_span(), "variable is a reference, so data it refers to cannot be written");
                                    Err(err)
                                }
                            }
                            _ => Err(self.fatal_error(env.env, "invalid expression")),
                        }
                    },
                    _ => Err(self.fatal_error(env.env, "invalid expression")),
                }
            },
            _ => Err(self.fatal_error(env.env, "invalid expression")),
        }
    }
}

/**
 * Evaluates an assignment expresson
 */
impl Eval for ExprAssign {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let addr = match (*self.left).eval_addr(env) {
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
        let val = (*self.right).eval(env)?;
        env.assign_var(addr, &val)?;
        Ok(Val::new())
    }
}

/**
 * Evaluates a binary expression.
 */
impl Eval for ExprBinary {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;
        self.op.eval(env, left, right, self.span)
    }
}

/**
 * Evaluates a block expression.
 */
impl Eval for ExprBlock {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        env.push_block(Scope::new(self.span))?;
        let ret_val = self.block.eval(env)?;
        env.pop_block()?;
        Ok(ret_val)
    }
}

/**
 * Evaluates break expression.
 */
impl Eval for ExprBreak {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::from_data(ValData::Break, None, self.span))
    }
}

/**
 * Evaluates a function call.
 */
impl Eval for ExprCall {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let mut values = Vec::new();
        for arg in &self.args {
            let val = arg.eval(env)?;
            values.push(val);
        }
        let item = env.load_item(&self.ident)?;
        match item.eval_func(values, env) {
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
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::from_data(ValData::Continue, None, self.span))
    }
}

/**
 * Evaluates to value by loading from memory using this identifier.
 */
impl Eval for ExprIdent {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        env.load_var(&self)
    }
}

/**
 * Evaluates an if statement.
 */
impl Eval for ExprIf {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let value = (*self.cond).eval(env)?;
        match value.get_bool() {
            Some(cond) => {
                if cond {
                    env.push_block(Scope::new(self.span))?;
                    let result = self.then_block.eval(env);
                    env.pop_block()?;
                    result
                } else {
                    match self.else_block.clone() {
                        Some(block) => {
                            env.push_block(Scope::new(self.span))?;
                            let result = block.eval(env);
                            env.pop_block()?;
                            result
                        },
                        None => Ok(Val::new()),
                    }
                }
            },
            None => Err(mismatched_types_fatal_error(env.env, value.span, TyKind::Bool, value.get_type())),
        }
    }
}

/**
 * Evaluates a literal expression.
 */
impl Eval for ExprLit {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        self.lit.eval(env)
    }
}

/**
 * Evaluates a parenthesized expression.
 */
impl Eval for ExprParen {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        (*self.expr).eval(env)
    }
}

/**
 * Evaluates a reference expression.
 */
impl Eval for ExprReference {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let val = (*self.expr).eval(env)?;
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
                    let addr = env.address_of(&ident, true)?;
                    let new_val = Val::from_ref(addr, ref_ty, self.mutable, self.span);
                    Ok(new_val)
                },
                None => {
                    // Reference to a new variable without identifier.
                    let addr = env.store_val(&val)?;
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
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match &*self.expr {
            Some(expr) => expr.eval(env),
            None => Ok(Val::from_data(ValData::Void, None, self.span)),
        }
    }
}

/**
 * Evaluates a unary expression.
 */
impl Eval for ExprUnary {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        let right = self.right.eval(env)?;
        self.op.eval(env, right, self.span)
    }
}

/**
 * Evaluates a while loop.
 */
impl Eval for ExprWhile {
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        loop {
            let value = (*self.cond).eval(env)?;
            match value.get_bool() {
                Some(cond) => {
                    if cond {
                        env.push_block(Scope::new(self.span))?;
                        let val = self.block.eval(env)?;
                        env.pop_block()?;
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
                    return Err(mismatched_types_fatal_error(env.env, value.span, TyKind::Bool, value.get_type()));
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
    fn eval(&self, env: &mut RuntimeEnv) -> IResult<Val> {
        match self {
            Lit::Int(literal) => literal.eval(env),
            Lit::Bool(literal) => literal.eval(env),
            Lit::Str(literal)  => Err(self.fatal_error(
                env, literal.span, "unsupported literal type by the interpreter")),
        }
    }
}

/**
 * Evaluates an int literal.
 */
impl Eval for LitInt {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::from_i32(self.value, self.span))
    }
}

/**
 * Evaluates a boolean literal.
 */
impl Eval for LitBool {
    fn eval(&self, _env: &mut RuntimeEnv) -> IResult<Val> {
        Ok(Val::from_bool(self.value, self.span))
    }
}
