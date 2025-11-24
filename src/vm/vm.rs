use log::error;
use crate::vm::{CodeChunk, Instruction, StackFrame, StackValue};
use crate::vm::memory::Heap;

pub struct TailVirtualMachine<'vm> {
    constant_pool: Vec<StackValue<'vm>>,
    op_stack: Vec<StackValue<'vm>>,
    call_stack: Vec<StackFrame<'vm>>,
    heap: Heap<'vm>,
}
impl <'vm> TailVirtualMachine<'vm> {
    pub fn new() -> Self {
        let mut call_stack = Vec::with_capacity(512);
        Self {
            constant_pool: Vec::with_capacity(64),
            op_stack: Vec::with_capacity(256),
            call_stack,
            heap: Heap::new(),
        }
    }

    pub fn add_to_constant_pool(&mut self, value: StackValue<'vm>) -> usize {
        self.constant_pool.push(value);
        self.constant_pool.len() - 1
    }

    pub fn run(&mut self, base_chunk: &'vm CodeChunk) {
        let mut ip = 0;
        self.push_frame(StackFrame::base(base_chunk));

        let mut code_chunk = self.call_stack.last().unwrap().code_chunk;
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
                Instruction::Ldc(index) => self.push(self.constant_pool[index].clone()),
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
                    let value = self.pop();
                    let first = self.read_as_int(&value);
                    let value = self.pop();
                    let second = self.read_as_int(&value);
                    self.push(StackValue::Int(first + second))
                }
                Instruction::ISub => {
                    let value = self.pop();
                    let first = self.read_as_int(&value);
                    let value = self.pop();
                    let second = self.read_as_int(&value);
                    self.push(StackValue::Int(first - second))
                }
                Instruction::IMul => {
                    let value = self.pop();
                    let first = self.read_as_int(&value);
                    let value = self.pop();
                    let second = self.read_as_int(&value);
                    self.push(StackValue::Int(first * second))
                }
                Instruction::IDiv => {
                    let value = self.pop();
                    let first = self.read_as_int(&value);
                    let value = self.pop();
                    let second = self.read_as_int(&value);
                    if second == 0 {
                        //error here. TODOD
                        error!("Divide by 0");
                    }
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
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test == 0 { next_ip = Some(index as usize); }
                }
                Instruction::JNe(index) => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test != 0 { next_ip = Some(index as usize); }
                }
                Instruction::JLt(index) => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test < 0 { next_ip = Some(index as usize); }
                }
                Instruction::JGt(index) => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test > 0 { next_ip = Some(index as usize); }
                }
                Instruction::JLe(index) => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test <= 0 { next_ip = Some(index as usize); }
                }
                Instruction::JGe(index) => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test >= 0 { next_ip = Some(index as usize); }
                }
                Instruction::Call => {
                    let StackValue::Function(arity, chunk) = self.pop() else {
                        panic!("Should never be reached")
                    };
                    let mut new_frame = StackFrame::new(chunk, ip + 1);
                    new_frame.slots[0] = Some(StackValue::Function(arity, chunk));
                    for i in 0..(arity as usize) {
                        new_frame.slots[i + 1] = Some(self.pop());
                    }
                    self.push_frame(new_frame);
                    next_ip = Some(0);
                }
                Instruction::Ret => {
                    let old_frame = self.pop_frame();
                    next_ip = Some(old_frame.return_address);
                }
                Instruction::Struct(elements) => {
                    let mut fields = Vec::with_capacity(elements as usize);
                    for _ in 0..elements {
                        fields.push(self.pop())
                    }
                    let strukt = StackValue::Struct(fields);
                    self.push(strukt);
                }
                Instruction::Alloc => {
                    let value = self.pop();
                    let pointer = self.heap.alloc(value);
                    self.push(StackValue::Ref(pointer));
                }
                Instruction::Pop => {
                    //discard
                    self.pop();
                }
            }
            if next_ip.is_none() {
                next_ip = Some(ip + 1);
            }
            ip = next_ip.unwrap();
            code_chunk = self.call_stack.last().unwrap().code_chunk;
        }
        println!("{:?}", self.heap);
        println!("{:?}", self.call_stack);
        println!("{:?}", self.op_stack);
    }
    
    // performs a dereference on the value provided. If the value is not a reference, it just returns
    fn read(&self, value: &'vm StackValue) -> &StackValue {
        match value {
            StackValue::Ref(addr) => self.heap.read(*addr),
            _ => value
        }
    }
    
    fn read_as_int(&self, value: &'vm StackValue) -> i64 {
        match value {
            // Circular references *should* never happen?
            StackValue::Ref(addr) => self.read_as_int(self.heap.read(*addr)),
            StackValue::Int(num) => *num,
            _ => panic!("Cant read non int as int")
        }
    }

    fn read_as_float(&self, value: &'vm StackValue) -> f64 {
        match value {
            // Circular references *should* never happen?
            StackValue::Ref(addr) => self.read_as_float(self.heap.read(*addr)),
            StackValue::Float(num) => *num,
            _ => panic!("Cant read non float as float")
        }
    }

    fn read_as_char(&self, value: &'vm StackValue) -> char {
        match value {
            // Circular references *should* never happen?
            StackValue::Ref(addr) => self.read_as_char(self.heap.read(*addr)),
            StackValue::Char(ch) => *ch,
            _ => panic!("Cant read non char as char")
        }
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