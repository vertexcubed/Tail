use std::collections::HashMap;
use std::fmt::{Display, Formatter};
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
impl Display for CaptureDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_local {
            write!(f, "Loc({})", self.slot)
        }
        else {
            write!(f, "Up({})", self.slot)
        }
    }
}

/// Definition of a function in a source file
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionDef {
    pub arity: u8,
    // index of the chunk of code corresponding to this function
    pub code_chunk: usize,
}
impl Display for FunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Function #{}", self.code_chunk)
    }
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
impl Display for StructDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {

        let mut fields = self.field_table.iter().collect::<Vec<_>>();
        fields.sort_by(|a, b| a.1.cmp(&b.1));
        let fields = fields.iter().map(|(k, _)| *k).collect::<Vec<_>>();

        write!(f, "{}(", self.name)?;
        for (i, k) in fields.iter().enumerate() {
            write!(f, "{}", **k)?;
            if i != fields.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}
// classes will behave largely the same

