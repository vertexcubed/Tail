use std::ops::{Index, IndexMut};
use crate::vm::memory::MemoryAddress;

pub mod vm;
mod memory;

#[derive(Debug, Clone)]
pub enum StackValue<'vm> {
    Int(i64),
    Float(f64),
    Char(char),
    Struct(Vec<StackValue<'vm>>),
    Ref(MemoryAddress),
    Function(u8, &'vm CodeChunk),
}
#[derive(Debug, Clone)]
pub struct CodeChunk {
    pub data: Vec<Instruction>
}
impl CodeChunk {
    pub fn new(data: Vec<Instruction>) -> Self {
        Self {
            data
        }
    }
}
impl Index<usize> for CodeChunk {
    type Output = Instruction;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

pub type Address = usize;


#[derive(Debug)]
pub struct StackFrame<'vm> {
    slots: Vec<Option<StackValue<'vm>>>,
    return_address: Address,
    is_base: bool,
    code_chunk: &'vm CodeChunk,
}
impl<'vm> StackFrame<'vm> {
    pub fn base(code_chunk: &'vm CodeChunk) -> Self {
        Self {
            slots: vec![None; 8],
            return_address: 0,
            is_base: true,
            code_chunk,
        }
    }

    pub fn new(code_chunk: &'vm CodeChunk, return_address: Address) -> Self {
        Self {
            slots: vec![None; 8],
            return_address,
            is_base: false,
            code_chunk,
        }
    }

    pub fn resize_if_needed(&mut self, index: usize) {
        let size = self.slots.len();
        if(index > size) {
            let new_size = f64::log2(index as f64 / size as f64).ceil() as usize;
            self.slots.resize(new_size, None);
        }
    }

    pub fn load(&self, index: usize) -> StackValue<'vm> {
        self.slots[index].clone().unwrap()
    }

    pub fn store(&mut self, index: usize, value: StackValue<'vm>) {
        self.resize_if_needed(index);
        self.slots[index] = Some(value);
    }
}



#[derive(Debug, Clone)]
pub enum Instruction {
    /// Pushes the int value -1 onto the stack.
    IPushM1,
    /// Pushes the int value 0 onto the stack.
    IPush0,
    /// Pushes the int value 1 onto the stack.
    IPush1,
    /// Pushes the int value 2 onto the stack.
    IPush2,
    /// Pushes the int value 3 onto the stack.
    IPush3,
    /// Pushes the int value 4 onto the stack.
    IPush4,
    /// Pushes the int value 5 onto the stack.
    IPush5,
    /// Pushes the int value provided onto the stack.
    IPush(i64),
    /// Pops the top value off the stack and discards
    Pop,
    /// Read a constant from the constant pool table
    Ldc(usize),
    /// Stores the top value on the stack into stack frame memory slot 0
    Store0,
    /// Stores the top value on the stack into stack frame memory slot 1
    Store1,
    /// Stores the top value on the stack into stack frame memory slot 2
    Store2,
    /// Stores the top value on the stack into stack frame memory slot 3
    Store3,
    /// Stores the top value on the stack into stack frame memory slot n
    Store(u8),
    /// Loads the value from stack frame memory slot 0 onto the stack
    Load0,
    /// Loads the value from stack frame memory slot 1 onto the stack
    Load1,
    /// Loads the value from stack frame memory slot 2 onto the stack
    Load2,
    /// Loads the value from stack frame memory slot 3 onto the stack
    Load3,
    /// Loads the value from stack frame memory slot n onto the stack
    Load(u8),
    /// Allocates the top value on the stack into the heap and pushes a pointer to it back on
    Alloc,
    /// Pops the first n elements off the stack and puts them in a struct, which is then pushed onto the stack
    Struct(u8),
    /// Gets the field of the struct currently on the stack. Needs to either be a Reference or a struct!
    // GetField(u16),
    IAdd,
    ISub,
    IMul,
    IDiv,
    Swap,
    Jump(u16),
    JEq(u16),
    JNe(u16),
    JLt(u16),
    JGt(u16),
    JLe(u16),
    JGe(u16),
    Call,
    Ret,
}