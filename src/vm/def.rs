use std::collections::HashMap;
use crate::vm::{Address, Identifier, TypeIdentifier};


#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct StackLoc {
    pub frame: usize,
    pub slot: usize
}

/// Represents a definition for a local to capture
#[derive(Debug, Copy, Clone)]
pub struct CaptureDef {
    // if true: local. false: upvalue in enclosing function
    pub is_local: bool,
    pub slot: u8,
}
impl CaptureDef {
    pub fn local(slot: u8) -> Self {
        Self {
            is_local: true,
            slot
        }
    }
    pub fn upvalue(slot: u8) -> Self {
        Self {
            is_local: false,
            slot
        }
    }
}

/// Definition of a function in a source file
#[derive(Debug)]
pub struct FunctionDef {
    pub arity: u8,
    // index of the chunk of code corresponding to this function
    pub code_chunk: usize,
}




/// A definition for a Struct type. Contains a table of all of its fields and methods.
/// Methods are NYI - TODO
/// Scoping NYI - TODO
#[derive(Debug)]
pub struct StructDef {
    pub name: TypeIdentifier,
    field_table: HashMap<Identifier, usize>,
    method_table: HashMap<Identifier, Address>,
}
impl StructDef {

    pub fn new(name: TypeIdentifier, fields: Vec<Identifier>) -> Self {
        let mut field_table = HashMap::with_capacity(fields.len());
        let mut i = 0;
        for id in fields.into_iter() {
            field_table.insert(id.clone(), i);
            i += 1;
        }
        Self {
            name,
            field_table,
            method_table: HashMap::new(),
        }
    }
    /// returns the index of a given field for a struct
    pub fn field_index(&self, name: Identifier) -> usize {
        self.field_table[&name]
    }

    pub fn field_len(&self) -> usize {
        self.field_table.len()
    }
}
// classes will behave largely the same

