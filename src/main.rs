use std::rc::Rc;
use crate::vm::{CodeChunk, StackValue};
use crate::vm::Instruction::*;
use crate::vm::vm::TailVirtualMachine;

mod vm;
mod ast;
mod ty;

fn main() {

    let mut vm = TailVirtualMachine::new();

    // let other_code = CodeChunk::new(vec![
    //     Load1,
    //     IPush(24),
    //     IMul,
    //     Ret,
    // ]);
    //
    // let other = StackValue::Function(1, &other_code);

    // let other_idx = vm.add_to_constant_pool(other);

    // let func_code = CodeChunk::new(vec![
    //     Load1,
    //     Ldc(other_idx),
    //     Call,
    //     Load2,
    //     IAdd,
    //     Ret,
    // ]);
    // let func = StackValue::Function(2, &func_code);

    // let func_idx = vm.add_to_constant_pool(func);

    // let a = 2
    // let b = 3
    // let c = b - a


    let instrs = vec![
        IPush2,
        Store0,
        IPush3,
        Store1,
        Load0,
        Load1,
        ISub,
        Store2
    ];
    let chunk = CodeChunk::new(instrs);

    vm.run(Rc::new(chunk));

    // func_code lives at least as long as the vm
}