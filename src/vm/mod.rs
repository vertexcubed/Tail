use std::ops::{Index, IndexMut};
use std::rc::Rc;
use crate::vm::def::StructDef;
use crate::vm::memory::MemoryAddress;

pub mod vm;
mod memory;
mod def;

#[derive(Debug, Clone)]
pub enum StackValue {
    Int(i64),
    Float(f64),
    Char(char),
    Struct(StructValue),
    Ref(MemoryAddress),
    Function(u8, Rc<CodeChunk>),
}
#[derive(Debug, Clone)]
pub struct StructValue {
    pub def: Rc<StructDef>,
    pub fields: Vec<Option<StackValue>>
}
impl  StructValue {

    /// Creates an empty struct value from the given def. Values are initialized to None but must NOT be none!
    pub fn from_def(def: Rc<StructDef>) -> Self {
        Self {
            fields: vec![None; def.field_len()],
            def
        }
    }
    /// Puts a field into the k specified. Key gotten from runtime constant pool
    pub fn put(&mut self, k: Identifier, value: StackValue) {
        let index = self.def.field_index(k);
        self.fields[index] = Some(value);
    }
    /// Gets a given struct field, clones it
    pub fn get(&self, k: Identifier) -> StackValue {
        let index = self.def.field_index(k);
        self.fields[index].clone().unwrap()
    }
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


// TODO: may change
pub type Address = usize;
pub type Identifier = String;
pub type TypeIdentifier = String;



#[derive(Debug)]
pub struct StackFrame {
    slots: Vec<Option<StackValue>>,
    return_address: Address,
    is_base: bool,
    code_chunk: Rc<CodeChunk>,
}
impl StackFrame {
    pub fn base(code_chunk: Rc<CodeChunk>) -> Self {
        Self {
            slots: vec![None; 8],
            return_address: 0,
            is_base: true,
            code_chunk,
        }
    }

    pub fn new(code_chunk: Rc<CodeChunk>, return_address: Address) -> Self {
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

    pub fn load(&self, index: usize) -> StackValue {
        self.slots[index].clone().unwrap()
    }

    pub fn store(&mut self, index: usize, value: StackValue) {
        self.resize_if_needed(index);
        self.slots[index] = Some(value);
    }
}

#[derive(Debug, Clone)]
pub enum ConstantPoolEntry {
    IntLit(i64),
    FloatLit(f64),
    CharLit(char),
    StringLit(String),
    StructDef(Rc<StructDef>),
    // Subject to change
    Identifier(Identifier),
}
impl  ConstantPoolEntry {
    pub fn as_stack_value(&self) -> StackValue {
        match self {
            ConstantPoolEntry::IntLit(i) => StackValue::Int(*i),
            ConstantPoolEntry::FloatLit(f) => StackValue::Float(*f),
            ConstantPoolEntry::CharLit(c) => StackValue::Char(*c),
            ConstantPoolEntry::StringLit(s) => todo!(),
            _ => panic!("Can't load non value as a stack value!"),
        }
    }
    pub fn as_struct_def(&self) -> Rc<StructDef> {
        match self {
            ConstantPoolEntry::StructDef(s) => s.clone(),
            _ => panic!("Can't load non-struct def as struct def!")
        }
    }
    
    pub fn as_identifier(&self) -> Identifier {
        match self {
            ConstantPoolEntry::Identifier(i) => i.clone(),
            _ => panic!("Can't load non-identifier as identifier!")
        }
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
    /// Duplicates the top value on the stack
    Dup,
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
    Store(usize),
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
    /// Pops the pointer on the stack and derefs, copying and pushing its value
    Deref,
    /// Pops the top value on off the stack and writes it into the memory address below it.
    Write,
    /// Creates an empty struct based on the struct def from the LDC
    Struct(usize),
    /// Pops the top struct off the top of the stack and gets the field from the LDC.
    GetField(usize),
    /// Pops the top value off the stack and sets the struct under it's field to this value
    SetField(usize),
    /// Pops the top ref struct off the top of the stack and gets the field from the LDC.
    GetFieldInd(usize),
    /// Pops the top value off the stack and sets the ref struct under it's field to this value
    SetFieldInd(usize),
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