use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use std::error::Error;

pub struct LLVMContext<'a> {
    context: &'a Context,
    builder: Builder<'a>,
    pass_manager: &'m PassManager<'a>,
    module: Module<'a>,
    function: &'a Function,
    variables: HashMap<String, PointerValue<'a>>,
    fn_return_value: Option<FunctionValue<'a>>,
}

pub fn compile_ir_with_llvm() {

}

fn create_llvm_context() {

}

fn create_entry_block_alloca(&self, name: &str) -> PointerValue {

}

