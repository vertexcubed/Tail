use crate::vm::{CodeChunk, ConstantPoolEntry, Instruction};

pub mod visit;


#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct RawConstantEntry {
    data: String,
    kind: RawConstantKind
}
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum RawConstantKind {
    Int,
    Float,
    Char,
    String,
    // TODO
    StructDef,
    Identifier,
}
impl Into<ConstantPoolEntry> for RawConstantEntry {
    fn into(self) -> ConstantPoolEntry {
        match self.kind {
            RawConstantKind::Int => ConstantPoolEntry::IntLit(self.data.parse::<i64>().unwrap()),
            RawConstantKind::Float => ConstantPoolEntry::FloatLit(self.data.parse::<f64>().unwrap()),
            RawConstantKind::Char => ConstantPoolEntry::CharLit(self.data.chars().next().unwrap()),
            RawConstantKind::String => ConstantPoolEntry::StringLit(self.data),
            RawConstantKind::StructDef => todo!(),
            RawConstantKind::Identifier => todo!(),
        }
    }
}


pub struct CodeChunkBuilder {
    data: Vec<Instruction>,
}
impl CodeChunkBuilder {
    pub fn new() -> Self {
        CodeChunkBuilder { data: Vec::new() }
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

    pub fn build(&mut self) -> CodeChunk {
        let data = std::mem::replace(&mut self.data, Vec::new());
        CodeChunk {
            data
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompileError {

}