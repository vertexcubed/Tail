use log::error;
use crate::vm::{CodeChunk, Instruction, StackFrame, StackValue};

pub struct TailVirtualMachine<'vm> {
    op_stack: Vec<StackValue<'vm>>,
    call_stack: Vec<StackFrame<'vm>>,
}
impl <'vm> TailVirtualMachine<'vm> {
    pub fn new() -> Self {
        let mut call_stack = Vec::with_capacity(512);
        call_stack.push(StackFrame::base());
        Self {
            op_stack: Vec::with_capacity(256),
            call_stack,
        }
    }

    pub fn run(&mut self, code_chunk: CodeChunk) {
        let mut ip = 0;

        while(ip < code_chunk.data.len()) {
            let mut next_ip = None;
            match code_chunk[ip] {
                Instruction::IPushM1 => self.push(StackValue::Int(-1)),
                Instruction::IPush0 => self.push(StackValue::Int(0)),
                Instruction::IPush1 => self.push(StackValue::Int(1)),
                Instruction::IPush2 => self.push(StackValue::Int(2)),
                Instruction::IPush3 => self.push(StackValue::Int(3)),
                Instruction::IPush4 => self.push(StackValue::Int(4)),
                Instruction::IPush5 => self.push(StackValue::Int(5)),
                Instruction::IPush(value) => self.push(StackValue::Int(value)),
                Instruction::Ldc(index) => {}
                Instruction::Store0 => {
                    let top = self.pop();
                    self.store(0, top);
                },
                Instruction::Store1 => {
                    let top = self.pop();
                    self.store(1, top);
                }
                Instruction::Store2 => {
                    let top = self.pop();
                    self.store(2, top);
                }
                Instruction::Store3 => {
                    let top = self.pop();
                    self.store(3, top);
                }
                Instruction::Store(index) => {
                    let top = self.pop();
                    self.store(index as usize, top);
                }
                Instruction::Load0 => {
                    let value = self.load(0);
                    self.push(value);
                }
                Instruction::Load1 => {
                    let value = self.load(1);
                    self.push(value);
                }
                Instruction::Load2 => {
                    let value = self.load(2);
                    self.push(value);
                }
                Instruction::Load3 => {
                    let value = self.load(3);
                    self.push(value);
                }
                Instruction::Load(index) => {
                    let value = self.load(index as usize);
                    self.push(value);
                }
                Instruction::IAdd => {
                    let second: i64 = self.pop().into();
                    let first: i64 = self.pop().into();
                    self.push(StackValue::Int(first + second))
                }
                Instruction::ISub => {
                    let second: i64 = self.pop().into();
                    let first: i64 = self.pop().into();
                    self.push(StackValue::Int(first - second))
                }
                Instruction::IMul => {
                    let second: i64 = self.pop().into();
                    let first: i64 = self.pop().into();
                    self.push(StackValue::Int(first * second))
                }
                Instruction::IDiv => {
                    let second: i64 = self.pop().into();
                    if second == 0 {
                        //error here. TODOD
                        error!("Divide by 0");
                    }
                    let first: i64 = self.pop().into();
                    self.push(StackValue::Int(first / second))
                }
                Instruction::Swap => {
                    let last = self.op_stack.len() - 1;
                    let temp = self.op_stack[last].clone();
                    self.op_stack[last] = self.op_stack[last - 1].clone();
                    self.op_stack[last - 1] = temp;
                }
                Instruction::Jump(index) => {
                    next_ip = Some(index as usize);
                }
                Instruction::JEq(index) => {
                    let to_test: i64 = self.pop().into();
                    if to_test == 0 { next_ip = Some(index as usize); }
                }
                Instruction::JNe(index) => {
                    let to_test: i64 = self.pop().into();
                    if to_test != 0 { next_ip = Some(index as usize); }
                }
                Instruction::JLt(index) => {
                    let to_test: i64 = self.pop().into();
                    if to_test < 0 { next_ip = Some(index as usize); }
                }
                Instruction::JGt(index) => {
                    let to_test: i64 = self.pop().into();
                    if to_test > 0 { next_ip = Some(index as usize); }
                }
                Instruction::JLe(index) => {
                    let to_test: i64 = self.pop().into();
                    if to_test <= 0 { next_ip = Some(index as usize); }
                }
                Instruction::JGe(index) => {
                    let to_test: i64 = self.pop().into();
                    if to_test >= 0 { next_ip = Some(index as usize); }
                }
            }
            if next_ip.is_none() {
                next_ip = Some(ip + 1);
            }
            ip = next_ip.unwrap();
        }
        
        println!("{:?}", self.call_stack);
        println!("{:?}", self.op_stack);
    }


    fn push_frame(&mut self, frame: StackFrame<'vm>) {
        self.call_stack.push(frame);
    }

    fn pop_frame(&mut self) -> StackFrame<'vm> {
        self.call_stack.pop().unwrap()
    }

    fn push(&mut self, value: StackValue<'vm>) {
        self.op_stack.push(value);
    }

    fn pop(&mut self) -> StackValue<'vm> {
        self.op_stack.pop().unwrap()
    }

    fn store(&mut self, slot: usize, value: StackValue<'vm>) {
        self.call_stack.last_mut().unwrap().store(slot, value);
    }

    fn load(&self, slot: usize) -> StackValue<'vm> {
        self.call_stack.last().unwrap().load(slot)
    }
}