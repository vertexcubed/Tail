use std::collections::HashMap;
use crate::vm::{StackValue, UpValue};
use crate::vm::def::StackLoc;

pub type MemoryAddress = usize;


/// Simple abstraction of a "heap" like object, so we can easily refer to things.
/// In practice, the implementation may change. For now it's just a vec.
#[derive(Debug)]
pub struct Heap {
    values: Vec<StackValue>,
}
impl  Heap {

    pub fn new() -> Self {
        Self {
            values: Vec::with_capacity(1024)
        }
    }

    pub fn alloc(&mut self, value: StackValue) -> MemoryAddress {
        let ret = self.values.len();
        self.values.push(value);
        ret
    }

    pub fn read(&self, index: MemoryAddress) -> &StackValue {
        &self.values[index]
    }

    pub fn read_mut(&mut self, index: MemoryAddress) -> &mut StackValue {
        &mut self.values[index]
    }

    pub fn write(&mut self, index: MemoryAddress, value: StackValue) {
        self.values[index] = value;
    }
}



pub struct UpValueStorage {
    upvalues: Vec<UpValue>,
    open_upvalues: HashMap<StackLoc, usize>
}
impl UpValueStorage {
    pub fn new() -> Self {
        Self {
            upvalues :Vec::new(),
            open_upvalues: HashMap::new()
        }
    }

    pub fn push_open_upvalue(&mut self, loc: StackLoc) -> usize {
        let idx = self.upvalues.len();
        let value = UpValue::Open(loc);
        self.upvalues.push(value);
        self.open_upvalues.insert(loc, idx);
        idx
    }

    pub fn get_open_upvalue_idx(&self, loc: &StackLoc) -> Option<usize> {
        self.open_upvalues.get(loc).cloned()
    }

    pub fn get(&self, index: usize) -> &UpValue {
        &self.upvalues[index]
    }

    pub fn get_mut(&mut self, index: usize) -> &mut UpValue {
        &mut self.upvalues[index]
    }

    pub fn close_upvalue(&mut self, index: usize, value: StackValue) {
        match &self.upvalues[index] {
            UpValue::Open(loc) => {
                self.open_upvalues.remove(loc);
            }
            UpValue::Closed(_) => {
                panic!("Cannot close an already closed upvalue!")
            }
        }
        self.upvalues[index] = UpValue::Closed(value);
    }
}