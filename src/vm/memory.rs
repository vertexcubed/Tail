use crate::vm::{StackValue};

pub type MemoryAddress = usize;


/// Simple abstraction of a "heap" like object, so we can easily refer to things.
/// In practice, the implementation may change. For now it's just a vec.
#[derive(Debug)]
pub struct Heap<'vm> {
    values: Vec<StackValue<'vm>>,
}
impl <'vm> Heap<'vm> {

    pub fn new() -> Self {
        Self {
            values: Vec::with_capacity(1024)
        }
    }

    pub fn alloc(&mut self, value: StackValue<'vm>) -> MemoryAddress {
        let ret = self.values.len();
        self.values.push(value);
        ret
    }

    pub fn read(&self, index: MemoryAddress) -> &StackValue<'vm> {
        &self.values[index]
    }

    pub fn write(&mut self, index: MemoryAddress, value: StackValue<'vm>) {
        self.values[index] = value;
    }
}