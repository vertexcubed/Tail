use crate::vm::def::{CaptureDef, FunctionDef, StackLoc, StructDef};
use crate::vm::memory::MemoryAddress;
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut, Index};
use std::rc::Rc;

pub mod vm;
#[allow(dead_code)]
pub mod memory;
#[allow(dead_code)]
pub mod def;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum StackValue {
    Int(i64),
    Float(f64),
    Char(char),
    Struct(StructValue),
    Ref(MemoryAddress),
    // index in constant pool of FunctionDef, followed by indices of upvalues
    Closure(usize, Rc<[usize]>),
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

// a reference to a captured value
#[derive(Debug, Clone)]
pub enum UpValue {
    Open(StackLoc),
    Closed(MemoryAddress)
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
pub type InstructionAddress = usize;
pub type Identifier = String;
pub type TypeIdentifier = String;

#[derive(Debug, Clone)]
pub struct StackSlot {
    // if this is captured, this is the upvalue index its captured at
    captured: Option<usize>,
    data: StackValue,
}
#[allow(dead_code)]
impl StackSlot {
    pub fn new(data: StackValue) -> Self {
        Self {
            captured: None,
            data,
        }
    }

    pub fn set_captured(&mut self, upvalue: usize) {
        if self.captured.is_some() {
            panic!("Can't capture stack slot twice!");
        }
        self.captured = Some(upvalue);
    }

    pub fn is_captured(&self) -> bool {
        self.captured.is_some()
    }
    pub fn upvalue_slot(&self) -> Option<usize> {
        self.captured.clone()
    }
}
impl Deref for StackSlot {
    type Target = StackValue;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for StackSlot {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl Into<StackValue> for StackSlot {
    fn into(self) -> StackValue {
        self.data
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum ConstantPoolEntry {
    IntLit(i64),
    FloatLit(f64),
    CharLit(char),
    StringLit(String),
    StructDef(Rc<StructDef>),
    FunctionDef(Rc<FunctionDef>),
    // Subject to change
    Identifier(Identifier),
}
impl ConstantPoolEntry {
    pub fn as_stack_value(&self) -> StackValue {
        match self {
            ConstantPoolEntry::IntLit(i) => StackValue::Int(*i),
            ConstantPoolEntry::FloatLit(f) => StackValue::Float(*f),
            ConstantPoolEntry::CharLit(c) => StackValue::Char(*c),
            ConstantPoolEntry::StringLit(_s) => todo!(),
            _ => panic!("Can't load non value as a stack value!"),
        }
    }

    pub fn as_func_def(&self) -> Rc<FunctionDef> {
        match self {
            ConstantPoolEntry::FunctionDef(f) => f.clone(),
            _ => panic!("Can't load non-function def as function def!")
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
impl Display for ConstantPoolEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantPoolEntry::IntLit(n) => write!(f, "{}", *n),
            ConstantPoolEntry::FloatLit(n) => write!(f, "{}", *n),
            ConstantPoolEntry::CharLit(c) => write!(f, "'{}'", *c),
            ConstantPoolEntry::StringLit(s) => write!(f, "\"{}\"", *s),
            ConstantPoolEntry::StructDef(strukt) => write!(f, "{}", *strukt),
            ConstantPoolEntry::FunctionDef(func) => write!(f, "{}", *func),
            ConstantPoolEntry::Identifier(id) => write!(f, "{}", id),
        }
    }
}




pub struct SourceFile {
    pub constant_pool: Vec<ConstantPoolEntry>,
    pub main_code: CodeChunk,
    pub functions: Vec<CodeChunk>,
}
#[allow(dead_code)]
impl SourceFile {
    pub fn get_func_code(&self, def: &FunctionDef) -> &CodeChunk {
        &self.functions[def.code_chunk]
    }

    pub fn get_func_by_index(&self, idx: usize) -> &CodeChunk {
        &self.functions[idx]
    }

    pub fn get_main_code(&self) -> &CodeChunk {
        &self.main_code
    }



    fn _write_chunk(before: &str, chunk: &CodeChunk, f: &mut Formatter<'_>) -> std::fmt::Result {
        for instr in chunk.data.iter() {
            write!(f, "{}{}\n", before, *instr)?;
        }
        Ok(())
    }
}
impl Display for SourceFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Source File\n==========\n")?;
        write!(f, "ConstantPool:\n")?;
        for (i, c) in self.constant_pool.iter().enumerate() {
            write!(f, "  {}: {}\n", i, c)?;
        }
        for (i, chunk) in self.functions.iter().enumerate() {
            write!(f, "Function #{}:\n", i)?;
            SourceFile::_write_chunk("  ", &chunk, f)?;
        }
        write!(f, "\nMain:\n")?;
        SourceFile::_write_chunk("  ", &self.main_code, f)?;

        write!(f, "==========")
    }
}





#[allow(dead_code)]
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
    IPush(u16),
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
    Load(usize),
    /// Stores the top value on the stack into global slot 0
    GStore0,
    /// Stores the top value on the stack into global slot 1
    GStore1,
    /// Stores the top value on the stack into global slot 2
    GStore2,
    /// Stores the top value on the stack into global slot 3
    GStore3,
    /// Stores the top value on the stack into global slot n
    GStore(usize),
    /// Loads the value from global slot 0 onto the stack
    GLoad0,
    /// Loads the value from global slot 1 onto the stack
    GLoad1,
    /// Loads the value from global slot 2 onto the stack
    GLoad2,
    /// Loads the value from global slot 3 onto the stack
    GLoad3,
    /// Loads the value from global slot n onto the stack
    GLoad(usize),
    /// Stores the top value on the stack into upvalue slot n
    UpStore(usize),
    /// Loads the value from upvalue slot n onto the stack
    UpLoad(usize),
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
    IMod,
    INeg,
    // returns 1 if top value = 0
    Eq,
    // returns 1 if top value != 0
    NEq,
    // returns 1 if top value < 0
    Lt,
    // etc
    Lte,
    Gt,
    Gte,
    BitAnd,
    BitOr,
    Swap,
    /// Unconditional jump
    Jump(u16),
    /// Jumps if top value is 0
    JEq(u16),
    /// Jumps if top value is not 0
    JNe(u16),
    /// makes a closure given the index into the constant pool for a function def, and a list of locals to capture
    ClosPush(usize, Vec<CaptureDef>),
    Call,
    Ret,
}
impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let as_str = match self {
            Instruction::IPushM1 => "IPushM1".to_string(),
            Instruction::IPush0 => "IPush0".to_string(),
            Instruction::IPush1 => "IPush1".to_string(),
            Instruction::IPush2 => "IPush2".to_string(),
            Instruction::IPush3 => "IPush3".to_string(),
            Instruction::IPush4 => "IPush4".to_string(),
            Instruction::IPush5 => "IPush5".to_string(),
            Instruction::IPush(n) => format!("IPush    {}", *n),
            Instruction::Pop => "Pop".to_string(),
            Instruction::Dup => "Dup".to_string(),
            Instruction::Ldc(n) => format!("Ldc   #{}", *n),
            Instruction::Store0 => "Store0".to_string(),
            Instruction::Store1 => "Store1".to_string(),
            Instruction::Store2 => "Store2".to_string(),
            Instruction::Store3 => "Store3".to_string(),
            Instruction::Store(n) => format!("Store    {}", *n),
            Instruction::Load0 => "Load0".to_string(),
            Instruction::Load1 => "Load1".to_string(),
            Instruction::Load2 => "Load2".to_string(),
            Instruction::Load3 => "Load3".to_string(),
            Instruction::Load(n) => format!("Load    {}", *n),
            Instruction::GStore0 => "GStore0".to_string(),
            Instruction::GStore1 => "GStore1".to_string(),
            Instruction::GStore2 => "GStore2".to_string(),
            Instruction::GStore3 => "GStore3".to_string(),
            Instruction::GStore(n) => format!("GStore    {}", *n),
            Instruction::GLoad0 => "GLoad0".to_string(),
            Instruction::GLoad1 => "GLoad1".to_string(),
            Instruction::GLoad2 => "GLoad2".to_string(),
            Instruction::GLoad3 => "GLoad3".to_string(),
            Instruction::GLoad(n) => format!("GLoad    {}", *n),
            Instruction::UpStore(n) => format!("UpStore    {}", *n),
            Instruction::UpLoad(n) => format!("UpLoad    {}", *n),
            Instruction::Alloc => "Alloc".to_string(),
            Instruction::Deref => "Deref".to_string(),
            Instruction::Write => "Write".to_string(),
            Instruction::Struct(s) => format!("Struct   #{}", *s),
            Instruction::GetField(f) => format!("GetField   #{}", *f),
            Instruction::SetField(f) => format!("SetField   #{}", *f),
            Instruction::GetFieldInd(f) => format!("GetFieldInd   #{}", *f),
            Instruction::SetFieldInd(f) => format!("SetFieldInd   #{}", *f),
            Instruction::IAdd => "IAdd".to_string(),
            Instruction::ISub => "ISub".to_string(),
            Instruction::IMul => "IMul".to_string(),
            Instruction::IDiv => "IDiv".to_string(),
            Instruction::IMod => "IMod".to_string(),
            Instruction::INeg => "INeg".to_string(),
            Instruction::Eq => "Eq".to_string(),
            Instruction::NEq => "NEq".to_string(),
            Instruction::Lt => "Lt".to_string(),
            Instruction::Lte => "Lte".to_string(),
            Instruction::Gt => "Gt".to_string(),
            Instruction::Gte => "Gte".to_string(),
            Instruction::BitAnd => "BitAnd".to_string(),
            Instruction::BitOr => "BitOr".to_string(),
            Instruction::Swap => "Swap".to_string(),
            Instruction::Jump(p) => format!("Jump   :{}", *p),
            Instruction::JEq(p) => format!("JEq   :{}", *p),
            Instruction::JNe(p) => format!("JNe   :{}", *p),
            Instruction::ClosPush(index, vec) => {
                let mut str = format!("ClosPush   #{} {}", *index, vec.len());
                for (i, c) in vec.iter().enumerate() {
                    str.push_str(format!("{}", c).as_str());
                    if i < vec.len() - 1 {
                        str.push_str(" ");
                    }
                }
                str
            },
            Instruction::Call => "Call".to_string(),
            Instruction::Ret => "Ret".to_string(),
        };
        write!(f, "{}", as_str)
    }
}