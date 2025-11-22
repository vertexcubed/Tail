use crate::vm::{CodeChunk, StackValue};
use crate::vm::Instruction::*;
use crate::vm::vm::TailVirtualMachine;

mod vm;

fn main() {

    let mut vm = TailVirtualMachine::new();

    let other_code = CodeChunk::new(vec![
        Load1,
        IPush(24),
        IMul,
        Ret,
    ]);

    let other = StackValue::Function(1, &other_code);

    let other_idx = vm.add_to_constant_pool(other);

    let func_code = CodeChunk::new(vec![
        Load1,
        Ldc(other_idx),
        Call,
        Load2,
        IAdd,
        Ret,
    ]);
    let func = StackValue::Function(2, &func_code);

    let func_idx = vm.add_to_constant_pool(func);

    let instrs = vec![
        IPush1,
        Store0,
        IPush2,
        Store1,
        IPush5,
        Store2,
        Load0,
        Load1,
        Ldc(func_idx),
        Call,
        Load2,
        Swap,
        ISub,
    ];
    let chunk = CodeChunk::new(instrs);

    vm.run(&chunk);

    // func_code lives at least as long as the vm
}