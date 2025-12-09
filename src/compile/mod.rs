use crate::vm::def::FunctionDef;
use crate::vm::{CodeChunk, ConstantPoolEntry, Instruction};
use std::rc::Rc;

pub mod visit;


#[derive(Debug, Clone, Hash, PartialEq, Eq)]
#[allow(dead_code)]
pub enum RawConstantEntry {
    Int(i64),
    // floats are not hashable so we do strings
    Float(String),
    Char(char),
    String(String),
    StructDef(()),
    Identifier(String),
    Function(FunctionDef)
}
impl Into<ConstantPoolEntry> for RawConstantEntry {
    fn into(self) -> ConstantPoolEntry {
        match self {
            RawConstantEntry::Int(data) => ConstantPoolEntry::IntLit(data),
            RawConstantEntry::Float(data) => ConstantPoolEntry::FloatLit(data.parse::<f64>().unwrap()),
            RawConstantEntry::Char(data) => ConstantPoolEntry::CharLit(data),
            RawConstantEntry::String(data) => ConstantPoolEntry::StringLit(data),
            RawConstantEntry::StructDef(_data) => todo!(),
            RawConstantEntry::Identifier(_data) => todo!(),
            RawConstantEntry::Function(data) => ConstantPoolEntry::FunctionDef(Rc::new(data)),
        }
    }
}


pub struct CodeChunkBuilder {
    index: CodeChunkIndex,
    data: Vec<Instruction>,
}
#[allow(dead_code)]
impl CodeChunkBuilder {
    pub fn root() -> Self {
        CodeChunkBuilder { 
            data: Vec::new(),
            index: CodeChunkIndex::Root,
        }
    }
    
    pub fn new(index: usize) -> Self {
        Self {
            data: Vec::new(),
            index: CodeChunkIndex::Function(index)
        }
    }

    pub fn push_instr(&mut self, instruction: Instruction) -> &mut Self {
        self.data.push(instruction);
        self
    }

    pub fn current_ip(&self) -> usize {
        self.data.len()
    }

    pub fn set(&mut self, ip: usize, instruction: Instruction) -> &mut Self {
        self.data[ip] = instruction;
        self
    }

    pub fn get_instr(&self, ip: usize) -> &Instruction {
        &self.data[ip]
    }
    pub fn get_instr_mut(&mut self, ip: usize) -> &mut Instruction {
        &mut self.data[ip]
    }
    
    pub fn last(&self) -> &Instruction {
        self.data.last().unwrap()
    }

    pub fn last_mut(&mut self) -> &mut Instruction {
        self.data.last_mut().unwrap()
    }

    pub fn build(&mut self) -> CodeChunk {
        let data = std::mem::replace(&mut self.data, Vec::new());
        CodeChunk {
            data
        }
    }
    
    pub fn is_root(&self) -> bool {
        CodeChunkIndex::Root == self.index
    }
    pub fn func_index(&self) -> CodeChunkIndex {
        self.index
    }
    
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum CodeChunkIndex {
    Root,
    Function(usize)
}

#[derive(Debug, Clone)]
pub enum CompileError {

}