use std::ops::{Index, IndexMut};

pub mod vm;

#[derive(Debug, Clone)]
pub enum StackValue<'vm> {
    Int(i64),
    Float(f64),
    Char(char),
    Function(u8, &'vm CodeChunk),
}
impl Into<i64> for StackValue<'_> {
    fn into(self) -> i64 {
        match self {
            StackValue::Int(i) => i,
            _ => panic!("Should never be reached by VM"),
        }
    }
}
impl Into<f64> for StackValue<'_> {
    fn into(self) -> f64 {
        match self {
            StackValue::Float(f) => f,
            _ => panic!("Should never be reached by VM"),
        }
    }
}
impl Into<char> for StackValue<'_> {
    fn into(self) -> char {
        match self {
            StackValue::Char(c) => c,
            _ => panic!("Should never be reached by VM"),
        }
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

pub type Address = u32;


#[derive(Debug)]
pub struct StackFrame<'vm> {
    slots: Vec<Option<StackValue<'vm>>>,
    return_address: Address,
    is_base: bool,
}
impl<'vm> StackFrame<'vm> {
    pub fn base() -> Self {
        Self {
            slots: vec![None; 8],
            return_address: 0,
            is_base: true
        }
    }

    pub fn new(return_address: Address) -> Self {
        Self {
            slots: vec![None; 8],
            return_address,
            is_base: false
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
    IPushM1,
    IPush0,
    IPush1,
    IPush2,
    IPush3,
    IPush4,
    IPush5,
    IPush(i64),
    Ldc(i32),
    Store0,
    Store1,
    Store2,
    Store3,
    Store(u8),
    Load0,
    Load1,
    Load2,
    Load3,
    Load(u8),
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
}