use crate::vm::{CodeChunk, Instruction};

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