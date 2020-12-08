use crate::lir::*;
use crate::interp::Value;


struct LirInterpContext {
    stack: Vec<u32>, // growing stack increases addresses, normally this decreases addresses
    sp: usize,
    bp: usize,
}

fn stack_mov_operand(lic: &mut LirInterpContext, dst: LirOperand, src: LirOperand) {
    match dst {
        LirOperand::DerefStackOffset(offset) => {
            lic.stack.reserve(bp + offset);
            lic.stack[] = match src {
                LirOperand::StackOffset(offset) => {
                LirOperand::StackOffset(offset) => {
                    Val::Ref(bp + offset)
                }
                
                LirOperand::DerefStackOffset(offset) => {
                    stack[bp + offset]
                }
                
                LirOperand::ConstantInt(val) => Val::Int(val),
                LirOperand::ConstantBool(val) => Val::Bool(val),
            }
        }
    }
}

pub fn interp_lir_instruction(lic: &mut LirInterpContext, insn: LirInstruction) {
    match insn.opcode {
        Nop => { },
        Mov => {
            stack_mov_value(
        },
        Add,
        Sub,
        Mul,
        Div,
        Pow,
        Mod,
        And,
        Or,
        Eq,
        Ne,
        Lt,
        Le,
        Gt,
        Ge,
        Not,
        Deref,
        IfLt,
        IfGt,
        IfLe,
        IfGe,
        IfEq,
        IfNe,
        Param,
        Call,
        Jump,
        Label,
        Prologue, // marks beginning of function
        Epilogue, // marks end of function

    }

}
