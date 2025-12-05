use std::rc::Rc;
use log::error;
use crate::vm::{CodeChunk, ConstantPoolEntry, Instruction, SourceFile, StackFrame, StackSlot, StackValue, StructValue, UpValue};
use crate::vm::def::StackLoc;
use crate::vm::memory::{Heap, UpValueStorage};


// src is the lifetime of the source file.
pub struct TailVirtualMachine<'src> {
    source_file: &'src SourceFile,
    upvalues: UpValueStorage,
    op_stack: Vec<StackValue>,
    globals: Vec<Option<StackValue>>,
    call_stack: Vec<StackFrame<'src>>,
    heap: Heap,
}
impl <'src> TailVirtualMachine<'src> {
    pub fn new(source_file: &'src SourceFile) -> Self {
        Self {
            source_file,
            op_stack: Vec::with_capacity(128),
            call_stack: Vec::with_capacity(512),
            globals: vec![None; 8],
            upvalues: UpValueStorage::new(),
            heap: Heap::new(),
        }
    }

    pub fn run(&mut self) {
        let mut ip = 0;
        self.push_frame(StackFrame::base(&self.source_file.main_code));

        let mut code_chunk = self.call_stack.last().unwrap().code_chunk;
        while(ip < code_chunk.data.len()) {
            let mut next_ip = None;

            // println!("{:?}", code_chunk[ip]);

            match &code_chunk[ip] {
                Instruction::IPushM1 => self.push(StackValue::Int(-1)),
                Instruction::IPush0 => self.push(StackValue::Int(0)),
                Instruction::IPush1 => self.push(StackValue::Int(1)),
                Instruction::IPush2 => self.push(StackValue::Int(2)),
                Instruction::IPush3 => self.push(StackValue::Int(3)),
                Instruction::IPush4 => self.push(StackValue::Int(4)),
                Instruction::IPush5 => self.push(StackValue::Int(5)),
                Instruction::IPush(value) => self.push(StackValue::Int(*value as i64)),
                Instruction::Ldc(index) => {
                    let value = self.load_constant(*index).as_stack_value();
                    self.push(value);
                },
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
                    self.store(*index, top);
                }
                Instruction::GStore0 => {
                    let top = self.pop();
                    self.store_global(0, top);
                }
                Instruction::GStore1 => {
                    let top = self.pop();
                    self.store_global(1, top);
                }
                Instruction::GStore2 => {
                    let top = self.pop();
                    self.store_global(2, top);
                }
                Instruction::GStore3 => {
                    let top = self.pop();
                    self.store_global(3, top);
                }
                Instruction::GStore(index) => {
                    let top = self.pop();
                    self.store_global(*index, top);
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
                    let value = self.load(*index as usize);
                    self.push(value);
                }
                Instruction::GLoad0 => {
                    let value = self.load_global(0);
                    self.push(value);
                }
                Instruction::GLoad1 => {
                    let value = self.load_global(1);
                    self.push(value);
                }
                Instruction::GLoad2 => {
                    let value = self.load_global(2);
                    self.push(value);
                }
                Instruction::GLoad3 => {
                    let value = self.load_global(3);
                    self.push(value);
                }
                Instruction::GLoad(index) => {
                    let value = self.load_global(*index);
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
                        //error here. TODO
                        error!("Divide by 0");
                    }
                    self.push(StackValue::Int(first / second))
                }
                Instruction::IMod => {
                    let value = self.pop();
                    let first = self.read_as_int(&value);
                    let value = self.pop();
                    let second = self.read_as_int(&value);
                    if second == 0 {
                        //error here. TODO
                        error!("Divide by 0");
                    }
                    self.push(StackValue::Int(first % second));
                }
                Instruction::INeg => {
                    let value = self.pop();
                    let num = self.read_as_int(&value);
                    self.push(StackValue::Int(-num));
                }
                Instruction::Swap => {
                    let last = self.op_stack.len() - 1;
                    let temp = self.op_stack[last].clone();
                    self.op_stack[last] = self.op_stack[last - 1].clone();
                    self.op_stack[last - 1] = temp;
                }
                Instruction::Jump(index) => {
                    next_ip = Some(*index as usize);
                }
                Instruction::JEq(index) => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test == 0 { next_ip = Some(*index as usize); }
                }
                Instruction::JNe(index) => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test != 0 { next_ip = Some(*index as usize); }
                }
                Instruction::Call => {
                    let StackValue::Closure(index, upvalues) = self.pop() else {
                        panic!("Should never be reached")
                    };
                    let function_def = self.load_constant(index).as_func_def();
                    let mut new_frame = StackFrame::new(
                        self.source_file.get_func_code(function_def.as_ref()),
                        ip + 1,
                        upvalues.clone()
                    );
                    new_frame.slots[0] = Some(StackSlot::new(StackValue::Closure(index, upvalues)));
                    for i in 0..(function_def.arity as usize) {
                        new_frame.slots[i + 1] = Some(StackSlot::new(self.pop()));
                    }
                    self.push_frame(new_frame);
                    next_ip = Some(0);

                    // let StackValue::Function(arity, chunk) = self.pop() else {
                    //     panic!("Should never be reached")
                    // };
                    // let mut new_frame = StackFrame::new(chunk.clone(), ip + 1);
                    // new_frame.slots[0] = Some(StackValue::Function(arity, chunk));
                    // for i in 0..(arity as usize) {
                    //     new_frame.slots[i + 1] = Some(self.pop());
                    // }
                    // self.push_frame(new_frame);
                    // next_ip = Some(0);
                }
                Instruction::Ret => {
                    // Close any possibly open upvalues
                    let mut old_frame = self.pop_frame();
                    for i in 0..old_frame.slots.len() {
                        self.clear_slot(&mut old_frame, i);
                    }

                    next_ip = Some(old_frame.return_address);
                }
                Instruction::Struct(index) => {
                    let def = self.load_constant(*index).as_struct_def();
                    let strukt = StructValue::from_def(def);
                    self.push(StackValue::Struct(strukt));
                }
                Instruction::GetField(index) => {
                    let strukt = match self.pop() {
                        StackValue::Struct(v) => v,
                        _ => panic!("Can't get field of non struct!")
                    };
                    let field = self.load_constant(*index).as_identifier();
                    let new_value = strukt.get(field);
                    self.push(new_value);
                }
                Instruction::SetField(index) => {
                    let to_set = self.pop();
                    let mut strukt = match self.pop() {
                        StackValue::Struct(v) => v,
                        _ => panic!("Can't set field of non struct!")
                    };
                    let field = self.load_constant(*index).as_identifier();
                    strukt.put(field, to_set);
                    self.push(StackValue::Struct(strukt));
                }
                Instruction::GetFieldInd(index) => {
                    let pointer = match(self.pop()) {
                        StackValue::Ref(addr) => match self.heap.read(addr) {
                            StackValue::Struct(strukt) => strukt,
                            _ => panic!("Can't get field of non struct!")
                        },
                        _ => panic!("Can't indirect get from a non reference!")
                    };
                    let field = self.load_constant(*index).as_identifier();
                    let new_value = pointer.get(field);
                    self.push(new_value);
                },
                Instruction::SetFieldInd(index) => {
                    let field = self.load_constant(*index).as_identifier();
                    let value = self.pop();
                    let addr = match self.pop() {
                        StackValue::Ref(addr) => addr,
                        _ => panic!("Can't indirect get from a non reference!")
                    };
                    let pointer = match self.heap.read_mut(addr) {
                        StackValue::Struct(strukt) => strukt,
                        _ => panic!("Can't set field of non struct!")
                    };
                    pointer.put(field, value);
                    self.push(StackValue::Ref(addr));
                },
                Instruction::Alloc => {
                    let value = self.pop();
                    let pointer = self.heap.alloc(value);
                    self.push(StackValue::Ref(pointer));
                }
                Instruction::Deref => {
                    let value = match self.pop() {
                        StackValue::Ref(addr) => self.heap.read(addr).clone(),
                        _ => panic!("Can't deref non reference!")
                    };
                    self.push(value);
                }
                Instruction::Write => {
                    let value = self.pop();
                    let addr = match self.pop() {
                        StackValue::Ref(addr) => addr,
                        _ => panic!("Can't deref non reference!")
                    };
                    self.heap.write(addr, value);
                }
                Instruction::Pop => {
                    //discard
                    self.pop();
                },
                Instruction::Dup => {
                    let value = self.peek().clone();
                    self.push(value);
                }
                Instruction::Eq => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test == 0 {
                        self.push(StackValue::Int(1));
                    }
                    else {
                        self.push(StackValue::Int(0));
                    }
                }
                Instruction::NEq => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test != 0 {
                        self.push(StackValue::Int(1));
                    }
                    else {
                        self.push(StackValue::Int(0));
                    }
                }
                Instruction::Lt => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test < 0 {
                        self.push(StackValue::Int(1));
                    }
                    else {
                        self.push(StackValue::Int(0));
                    }
                }
                Instruction::Lte => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test <= 0 {
                        self.push(StackValue::Int(1));
                    }
                    else {
                        self.push(StackValue::Int(0));
                    }
                }
                Instruction::Gt => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test > 0 {
                        self.push(StackValue::Int(1));
                    }
                    else {
                        self.push(StackValue::Int(0));
                    }
                }
                Instruction::Gte => {
                    let value = self.pop();
                    let to_test = self.read_as_int(&value);
                    if to_test >= 0 {
                        self.push(StackValue::Int(1));
                    }
                    else {
                        self.push(StackValue::Int(0));
                    }
                }
                Instruction::BitAnd => {}
                Instruction::BitOr => {}
                Instruction::ClosPush(index, to_capture) => {
                    let mut upvalues = Vec::with_capacity(to_capture.len());
                    for c in to_capture.iter() {
                        // if true, this corresponds to a local var. we need to make an upvalue corresponding to this
                        if c.is_local {
                            let idx = self.get_or_create_upvalue(c.slot as usize);
                            upvalues.push(idx);
                        }
                        else {
                            // if not, this corresponds to an upvalue in the enclosing function
                            upvalues.push(self.local_upvalue_idx(c.slot as usize));
                        }
                    }
                    self.push(StackValue::Closure(*index, upvalues));
                }
                Instruction::UpStore(index) => 'upstore: {
                    // ugly to prevent multiple mut references :')
                    let value = self.pop();
                    let upvalue_index = self.local_upvalue_idx(*index);
                    let upvalue = self.upvalues.get(upvalue_index);
                    if let UpValue::Closed(old_value) = upvalue {
                        self.heap.write(*old_value, value);
                        break 'upstore;
                    }
                    let UpValue::Open(loc) = upvalue.clone() else {
                        unreachable!()
                    };
                    self.store_loc(&loc, value);
                }
                Instruction::UpLoad(index) => {
                    let upvalue_index = self.local_upvalue_idx(*index);
                    let to_push = match self.upvalues.get(upvalue_index) {
                        UpValue::Closed(value) => self.heap.read(value.clone()).clone(),
                        UpValue::Open(loc) => self.load_loc(loc)
                    };
                    self.push(to_push);
                }
            }
            if next_ip.is_none() {
                next_ip = Some(ip + 1);
            }
            ip = next_ip.unwrap();
            code_chunk = self.call_stack.last().unwrap().code_chunk;
        }
        println!("Op Stack: {:?}", self.op_stack);
        println!("Heap: {:?}", self.heap);
        println!("Call Stack: {:?}", self.call_stack);
        println!("Globals: {:?}", self.globals);
        println!("Current locals: {:?}", self.call_stack.last().unwrap().slots);
        println!("Upvalues: {:?}", self.upvalues)
    }
    
    fn read_as_int(&self, value: &StackValue) -> i64 {
        match value {
            // Circular references *should* never happen?
            StackValue::Ref(addr) => self.read_as_int(self.heap.read(*addr)),
            StackValue::Int(num) => *num,
            _ => panic!("Cant read non int as int")
        }
    }

    fn read_as_float(&self, value: &StackValue) -> f64 {
        match value {
            // Circular references *should* never happen?
            StackValue::Ref(addr) => self.read_as_float(self.heap.read(*addr)),
            StackValue::Float(num) => *num,
            _ => panic!("Cant read non float as float")
        }
    }

    fn read_as_char(&self, value: &StackValue) -> char {
        match value {
            // Circular references *should* never happen?
            StackValue::Ref(addr) => self.read_as_char(self.heap.read(*addr)),
            StackValue::Char(ch) => *ch,
            _ => panic!("Cant read non char as char")
        }
    }
    
    fn push_frame(&mut self, frame: StackFrame<'src>) {
        self.call_stack.push(frame);
    }

    fn pop_frame(&mut self) -> StackFrame<'src> {
        self.call_stack.pop().unwrap()
    }

    fn push(&mut self, value: StackValue) {
        self.op_stack.push(value);
    }

    fn pop(&mut self) -> StackValue {
        self.op_stack.pop().unwrap()
    }

    fn peek(&self) -> &StackValue {
        self.op_stack.last().unwrap()
    }
    fn peek_mut(&mut self) -> &mut StackValue {
        self.op_stack.last_mut().unwrap()
    }

    fn store(&mut self, slot: usize, value: StackValue) {
        self.call_stack.last_mut().unwrap().store(slot, value);
    }

    fn store_loc(&mut self, loc: &StackLoc, value: StackValue) {
        **self.get_slot_mut(loc) = value;
    }

    fn store_global(&mut self, index: usize, value: StackValue) {
        // resize if needed
        let size = self.globals.len();
        if(index > size) {
            let new_size = f64::log2(index as f64 / size as f64).ceil() as usize;
            self.globals.resize(new_size, None);
        }

        self.globals[index] = Some(value);
    }

    fn store_ind(&mut self, slot: usize, value: StackValue) {
        let addr = match self.load(slot) {
            StackValue::Ref(addr) => addr,
            _ => panic!("Can't indirect store into non reference!")
        };
        self.heap.write(addr, value);
    }

    fn load(&self, slot: usize) -> StackValue {
        self.call_stack.last().unwrap().load(slot)
    }

    fn load_loc(&self, loc: &StackLoc) -> StackValue {
        (**self.get_slot(loc)).clone()
    }

    fn load_global(&self, index: usize) -> StackValue {
        self.globals[index].as_ref().unwrap().clone()
    }

    fn load_constant(&self, index: usize) -> &'src ConstantPoolEntry {
        &self.source_file.constant_pool[index]
    }

    fn local_upvalue_idx(&self, index: usize) -> usize {
       self.call_stack.last().unwrap().upvalues[index]
    }

    fn clear_slot(&mut self, frame: &mut StackFrame, index: usize) {
        // clears the slot and gives us the value that was there
        let Some(slot) = std::mem::replace(&mut frame.slots[index], None) else {
            return;
        };
        if !slot.is_captured() {
            return;
        }
        // this local was captured. close the upvalue
        let upvalue_index = slot.upvalue_slot().unwrap();
        let data: StackValue = slot.into();
        let heap_addr = self.heap.alloc(data);


        self.upvalues.close_upvalue(upvalue_index, heap_addr);
    }

    /// get or create an upvalue for a local variable. If it already exists, we return the index instead of making a new one.
    fn get_or_create_upvalue(&mut self, slot: usize) -> usize {
        let loc = StackLoc {
            frame: self.call_stack.len() - 1,
            slot,
        };
        match self.upvalues.get_open_upvalue_idx(&loc) {
            None => {
                let idx = self.upvalues.push_open_upvalue(loc);
                self.get_slot_mut(&loc).set_captured(idx);
                idx
            },
            Some(idx) => idx
        }
    }

    fn get_slot(&self, loc: &StackLoc) -> &StackSlot {
        self.call_stack[loc.frame].slots[loc.slot].as_ref().unwrap()
    }
    fn get_slot_mut(&mut self, loc: &StackLoc) -> &mut StackSlot {
        self.call_stack[loc.frame].slots[loc.slot].as_mut().unwrap()
    }
}