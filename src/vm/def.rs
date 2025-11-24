use std::collections::HashMap;
use crate::vm::{Address, Identifier, TypeIdentifier};

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