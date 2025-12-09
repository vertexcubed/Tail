use crate::vm::def::{FunctionDef, StackLoc};
use crate::vm::memory::{CallStack, Heap, UpValueStorage};
use crate::vm::{ConstantPoolEntry, Instruction, InstructionAddress, SourceFile, StackSlot, StackValue, StructValue, UpValue};
use log::error;
use std::rc::Rc;


// src is the lifetime of the source file.
pub struct TailVirtualMachine<'src> {
    source_file: &'src SourceFile,

    call_stack: CallStack,
    upvalues: UpValueStorage,
    op_stack: Vec<StackValue>,
    globals: Vec<Option<StackValue>>,
    heap: Heap,
}


impl <'src> TailVirtualMachine<'src> {
    pub fn new(source_file: &'src SourceFile) -> Self {
        Self {
            source_file,
            op_stack: Vec::with_capacity(1024 * 1024),
            globals: vec![None; 256],
            upvalues: UpValueStorage::new(),
            heap: Heap::new(),
            call_stack: CallStack::new(),
        }
    }

    pub fn run(&mut self) {
        let mut ip = 0;

        let mut code_chunk = self.source_file.get_main_code();
        while ip < code_chunk.data.len() {
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
                    self.push(value.clone());
                }
                Instruction::Load1 => {
                    let value = self.load(1);
                    self.push(value.clone());
                }
                Instruction::Load2 => {
                    let value = self.load(2);
                    self.push(value.clone());
                }
                Instruction::Load3 => {
                    let value = self.load(3);
                    self.push(value.clone());
                }
                Instruction::Load(index) => {
                    let value = self.load(*index as usize);
                    self.push(value.clone());
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
                    // let mut new_frame = StackFrame::new(
                    //     self.source_file.get_func_code(function_def.as_ref()),
                    //     ip + 1,
                    //     upvalues.clone()
                    // );
                    // new_frame.slots[0] = Some(StackSlot::new(StackValue::Closure(index, upvalues)));
                    // for i in 0..(function_def.arity as usize) {
                    //     new_frame.slots[i + 1] = Some(StackSlot::new(self.pop()));
                    // }
                    self.push_frame(ip + 1, upvalues.clone(), function_def.as_ref());
                    self.store(0, StackValue::Closure(index, upvalues));
                    for i in 0..(function_def.arity as usize) {
                        let v = self.pop();
                        self.store(i + 1, v);
                    }

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
                    // TODO: this is very slow and needs to be optimized
                    for s in self.call_stack.slot_iter_mut() {
                        // clears the slot and gives us the value that was there
                        let Some(old_slot) = std::mem::replace(s, None) else {
                            continue;
                        };
                        if !old_slot.is_captured() {
                            continue;
                        }
                        // this local was captured. close the upvalue
                        let upvalue_index = old_slot.upvalue_slot().unwrap();
                        let data: StackValue = old_slot.into();
                        let heap_addr = self.heap.alloc(data);

                        self.upvalues.close_upvalue(upvalue_index, heap_addr);
                    }

                    next_ip = Some(self.pop_frame());
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
                    let pointer = match self.pop() {
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
                    self.push(StackValue::Closure(*index, upvalues.into_boxed_slice().into()));
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
            code_chunk = if self.call_stack.is_main() {
                self.source_file.get_main_code()
            }
            else {
                let idx = self.call_stack.current_code();
                self.source_file.get_func_by_index(idx)
            }
        }
    }

    pub fn _print_state(&self) {
        println!("Op Stack: {:?}", self.op_stack);
        println!("Heap: {:?}", self.heap);
        let mut local_str = String::from("Locals: [ | ");
        for (i, g) in self.call_stack.slot_iter().enumerate() {
            if !g.is_none() {
                local_str.push_str(format!("{}: {:?} | ", i, g.as_ref().unwrap().data).as_str());
            }
        }
        local_str.push_str("]");
        println!("{}", local_str);

        let mut global_str = String::from("Globals: [ | ");
        for (i, g) in self.globals.iter().enumerate() {
            if !g.is_none() {
                global_str.push_str(format!("{}: {:?} | ", i, g.as_ref().unwrap()).as_str());
            }
        }
        global_str.push_str("]");
        println!("{}", global_str);

        // println!("Current locals: {:?}", self.call_stack.last().unwrap().slots);
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

    fn push_frame(&mut self, return_addr: InstructionAddress, upvalues: Rc<[usize]>, function: &FunctionDef) {
        self.call_stack.push_frame(return_addr, upvalues, function.code_chunk);
    }
    fn pop_frame(&mut self) -> usize {
        self.call_stack.pop_frame().return_addr
    }

    fn store(&mut self, slot: usize, value: StackValue) {
        self.call_stack.store(slot, value);
    }

    fn store_loc(&mut self, loc: &StackLoc, value: StackValue) {
        **self.get_slot_mut(loc) = value;
    }

    fn store_global(&mut self, index: usize, value: StackValue) {
        // resize if needed
        let size = self.globals.len();
        if index > size {
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
        self.heap.write(*addr, value);
    }

    fn load(&self, slot: usize) -> &StackValue {
        self.call_stack.load(slot)
    }
    fn load_mut(&mut self, slot: usize) -> &mut StackValue {
        self.call_stack.load_mut(slot)
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
       self.call_stack.upvalue_index(index)
    }

    /// get or create an upvalue for a local variable. If it already exists, we return the index instead of making a new one.
    fn get_or_create_upvalue(&mut self, slot: usize) -> usize {
        let loc = StackLoc {
            frame: self.call_stack.frame_len(),
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
        self.call_stack.get_slot(loc)
    }
    fn get_slot_mut(&mut self, loc: &StackLoc) -> &mut StackSlot {
        self.call_stack.get_slot_mut(loc)
    }
}