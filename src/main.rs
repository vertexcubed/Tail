use crate::vm::CodeChunk;
use crate::vm::Instruction::*;
use crate::vm::vm::TailVirtualMachine;

mod vm;

fn main() {

    let instrs = vec![
        IPush5, //0
        Store0, //1
        Load0, //2
        IPush4, //3
        ISub, //4
        JGe(8), //5
        IPush2, //6
        Jump(9), //7
        IPush(17), //8
        Store1, //9
    ];
    let chunk = CodeChunk::new(instrs);
    let mut vm = TailVirtualMachine::new();
    vm.run(chunk);
}