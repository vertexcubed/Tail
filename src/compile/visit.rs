use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use crate::ast::{BinOp, Block, Expr, ExprKind, FuncBlock, Identifier, Literal, Stmt, StmtKind, UOp};
use crate::ast::visit::AstVisitor;
use crate::compile::{CodeChunkBuilder, CompileError, RawConstantEntry, RawConstantKind};
use crate::vm::{CodeChunk, Instruction};


pub struct Compiler {
    core: Rc<RefCell<CompilerCore>>,
    pub root: FrameCompiler,
}
impl Compiler {
    pub fn new() -> Self {
        let core = Rc::new(RefCell::new(CompilerCore::new()));
        Self {
            root: FrameCompiler::new(Rc::downgrade(&core), true),
            core
        }
    }
}

pub struct CompilerCore {
    // we hash by the constant entry, returning the index that's then stuck into the bytecode
    constant_pool: HashMap<RawConstantEntry, usize>,
    next_pool_slot: usize,
}
impl CompilerCore {
    pub fn new() -> Self {
        Self {
            constant_pool: HashMap::new(),
            next_pool_slot: 0,
        }
    }

    pub fn reserve_pool_slot(&mut self) -> usize {
        let slot = self.next_pool_slot;
        self.next_pool_slot += 1;
        slot
    }
}













// compiles a single stack frame
pub struct FrameCompiler {
    parent: Weak<RefCell<CompilerCore>>,
    pub chunk: CodeChunkBuilder,
    // if true, this is the root frame compiler. scope 0 = Global
    is_root: bool,
    frame_variable_slots: HashMap<Identifier, LocStack>,
    next_var_slot: usize,
    scope_level: usize,
}
impl FrameCompiler {
    pub fn new(parent: Weak<RefCell<CompilerCore>>, is_root: bool) -> Self {
        Self {
            chunk: CodeChunkBuilder::new(),

            frame_variable_slots: HashMap::new(),
            is_root,
            parent,
            next_var_slot: 0,
            // level 0 = global scope
            scope_level: 0
        }
    }

    pub fn reserve_var_slot(&mut self) -> usize {
        let slot = self.next_var_slot;
        self.next_var_slot += 1;
        slot
    }

    pub fn trim_locs(&mut self, id: &Identifier) {
        if let Some(loc) = self.frame_variable_slots.get_mut(id) {
            while self.scope_level < loc.peek().scope {
                loc.pop();
            }
            if loc.len() == 0 {
                self.frame_variable_slots.remove(id);
            }
        }
    }

    fn push_bop_instr(&mut self, bop: &BinOp) -> Result<(), CompileError> {
        // TODO: operator overloading :')
        let instrs = match bop {
            BinOp::Add => vec![Instruction::IAdd],
            BinOp::Sub => vec![Instruction::ISub],
            BinOp::Mul => vec![Instruction::IMul],
            BinOp::Div => vec![Instruction::IDiv],
            BinOp::Mod => vec![Instruction::IMod],
            BinOp::And => vec![Instruction::BitAnd],
            BinOp::Or => vec![Instruction::BitOr],
            BinOp::Lt => vec![Instruction::ISub, Instruction::Lt],
            BinOp::Gt => vec![Instruction::ISub, Instruction::Gt],
            BinOp::Lte => vec![Instruction::ISub, Instruction::Lte],
            BinOp::Gte => vec![Instruction::ISub, Instruction::Gte],
            BinOp::Eq => vec![Instruction::ISub, Instruction::Eq],
            BinOp::NEq => vec![Instruction::ISub, Instruction::NEq],
        };
        for i in instrs.into_iter() {
            self.chunk.push_instr(i);
        }
        Ok(())
    }
}

impl AstVisitor for FrameCompiler {
    type ExprResult = Result<(), CompileError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::ExprResult {
        match &stmt.kind {
            // lets are pretty straightforward: we push the value then store, though now we need to keep track of slots.
            StmtKind::Let(id, body) => {
                // visit the body expr, which pushes its result onto the stack
                self.visit_expr(body)?;
                // then, we store it into the next available slot.
                // cases:
                //      does not exist: just add a new value, easy
                //      current scope < top scope: repeatedly pop until current >= top_scope
                //      current scope = top scope: modify the slot to next available slot
                //      current scope > top scope: push a new scope, store in next available slot
                let slot = self.reserve_var_slot();
                let loc_type;

                // remove locs that are out of scope
                self.trim_locs(id);
                if let Some(loc) = self.frame_variable_slots.get_mut(id) {
                    let top = loc.peek();
                    if self.scope_level == top.scope {
                        loc.replace_top(VarLoc::new(top.loc_type, slot, top.scope));
                    }
                    else {
                        loc.push(VarLoc::new(LocType::Local, slot, self.scope_level));
                    }
                    loc_type = top.loc_type;
                }
                else {
                    // easy: add a new value
                    loc_type = LocType::Local;
                    self.frame_variable_slots.insert(id.clone(), LocStack::new(VarLoc::new(LocType::Local, slot, self.scope_level)));
                }

                // make the instruction
                // if scope level is 0, we're in the global scope. push a GStore/Load instr instead.
                let instr = if let LocType::Local = loc_type {
                    match slot {
                        0 => Instruction::GStore0,
                        1 => Instruction::GStore1,
                        2 => Instruction::GStore2,
                        3 => Instruction::GStore3,
                        _ => Instruction::GStore(slot)
                    }
                } else {
                    match slot {
                        0 => Instruction::Store0,
                        1 => Instruction::Store1,
                        2 => Instruction::Store2,
                        3 => Instruction::Store3,
                        _ => Instruction::Store(slot)
                    }
                };
                // push it
                self.chunk.push_instr(instr);
                Ok(())

            }
            StmtKind::Assign(place, body) => {
                todo!()
            }
            // expressions are easy: just compile, then void the output (pop it off the stack)
            StmtKind::Expr(e) => {
                self.visit_expr(e)?;
                self.chunk.push_instr(Instruction::Pop);
                Ok(())
            }
            // returns are also easy: just compile the expr then push a "ret" instruction
            StmtKind::Ret(body) => {
                if let Some(e) = body {
                    self.visit_expr(e)?;
                }
                self.chunk.push_instr(Instruction::Ret);
                Ok(())
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        match &expr.kind {
            ExprKind::Ident(id) => {
                // remove locs that are out of scope
                self.trim_locs(id);

                let Some(loc) = self.frame_variable_slots.get_mut(id) else {
                    // could not find variable in the current scope - this is either global, or needs to be captured.
                    // TODO
                    return Ok(())
                };
                let loc = loc.peek();
                // if scope of this var is global, then we load a GLoad instr instead
                let instr = if let LocType::Global = loc.loc_type {
                    match loc.slot {
                        0 => Instruction::GLoad0,
                        1 => Instruction::GLoad1,
                        2 => Instruction::GLoad2,
                        3 => Instruction::GLoad3,
                        _ => Instruction::GLoad(loc.slot)
                    }
                } else {
                    match loc.slot {
                        0 => Instruction::Load0,
                        1 => Instruction::Load1,
                        2 => Instruction::Load2,
                        3 => Instruction::Load3,
                        _ => Instruction::Load(loc.slot)
                    }
                };
                self.chunk.push_instr(instr);
                Ok(())
            },
            ExprKind::Lit(lit) => self.visit_literal(lit),
            ExprKind::UnaryOp(uop, operand) => {
                self.visit_expr(operand)?;
                let instr = match uop {
                    // boolean not actually behaves identically to Eq -> if 0, push 1. if not 0, push 0.
                    UOp::Not => Instruction::Eq,
                    // TODO: operator overloading
                    UOp::Neg => Instruction::INeg,
                };
                self.chunk.push_instr(instr);
                Ok(())
            },

            ExprKind::BinaryOp(bop, left, right) => {
                // TODO: Update calling convention to be left -> right not right -> left (unintuitive)
                self.visit_expr(right)?;
                self.visit_expr(left)?;
                self.push_bop_instr(bop)
            },
            ExprKind::If(cond, then_branch, else_branch) => {
                self.visit_expr(cond)?;
                // this is used to later write the correct jump address
                let cond_jump_ip = self.chunk.current_ip();
                self.chunk.push_instr(Instruction::JEq(0)); // temp address

                // compile then branch
                self.visit_block(then_branch)?;
                // also used later to write the correct jump address
                let finish_jump_ip = self.chunk.current_ip();
                self.chunk.push_instr(Instruction::Jump(0)); // temp address
                // compile else branch
                self.visit_block(else_branch)?;

                let end_if_ip = self.chunk.current_ip();

                // finish_jump_ip + 1 = start of else branch
                *self.chunk.get_instr_mut(cond_jump_ip) = Instruction::JEq((finish_jump_ip + 1) as u16);
                // end_if_ip = end of the if statement
                *self.chunk.get_instr_mut(finish_jump_ip) = Instruction::Jump(end_if_ip as u16);

                Ok(())
            },
            ExprKind::Closure(name, args, body) => {



                Ok(())
            }
            ExprKind::Call(_, _) => todo!(),
            ExprKind::Ref(e) => {
                self.visit_expr(e)?;
                self.chunk.push_instr(Instruction::Alloc);
                Ok(())
            },
            ExprKind::Block(block) => {
                self.visit_block(block)
            },
            ExprKind::Struct(_) => todo!(),
            ExprKind::Place(_) => todo!(),
        }
    }

    fn visit_ident(&mut self, ident: &Identifier) -> Self::ExprResult {
        unreachable!("Unused")
    }

    fn visit_block(&mut self, block: &Block) -> Self::ExprResult {
        todo!()
    }

    // blocks create a *new* code chunk
    fn visit_func_block(&mut self, block: &FuncBlock) -> Self::ExprResult {
        todo!()
    }

    // literals are pretty simple: read a value from the constant pool (add it if it doesnt exist)
    fn visit_literal(&mut self, literal: &Literal) -> Self::ExprResult {
        let instr = match literal {
            // hardcoded int pushes
            Literal::Int(-1) => Instruction::IPushM1,
            Literal::Int(0) => Instruction::IPush0,
            Literal::Int(1) => Instruction::IPush1,
            Literal::Int(2) => Instruction::IPush2,
            Literal::Int(3) => Instruction::IPush3,
            Literal::Int(4) => Instruction::IPush4,
            Literal::Int(5) => Instruction::IPush5,
            // small enough to store in a u16, which can use IPush(u16)
            Literal::Int(i) if *i > 5 && *i < 32768 => Instruction::IPush(*i as u16),
            Literal::Int(i) => {
                // if else, we need to load this into the constant pool
                let entry = RawConstantEntry {
                data: (*i).to_string(),
                    kind: RawConstantKind::Int,
                };
                let core = self.parent.upgrade().unwrap();
                let mut core = core.borrow_mut();

                // if entry doesn't exist, add it.
                if !core.constant_pool.contains_key(&entry) {
                    let slot = core.reserve_pool_slot();
                    core.constant_pool.insert(entry.clone(), slot);
                }

                Instruction::Ldc(core.constant_pool.get(&entry).unwrap().clone())
            }
            Literal::Float(f) => todo!(),
            Literal::Char(c) => todo!(),
            Literal::Str(s) => todo!(),
            Literal::Bool(true) => Instruction::IPush1,
            Literal::Bool(false) => Instruction::IPush0,
        };
        self.chunk.push_instr(instr);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LocStack {
    locs: Vec<VarLoc>,
}
impl LocStack {
    pub fn new(first: VarLoc) -> Self {
        Self {
            locs: vec![first],
        }
    }

    pub fn peek(&self) -> VarLoc {
        *self.locs.last().unwrap()
    }

    pub fn pop(&mut self) -> VarLoc {
        debug_assert!(self.locs.len() > 1);
        self.locs.pop().unwrap()
    }

    pub fn push(&mut self, loc: VarLoc) {
        self.locs.push(loc);
    }

    pub fn len(&self) -> usize {
        self.locs.len()
    }
    pub fn replace_top(&mut self, loc: VarLoc) -> VarLoc {
        std::mem::replace(&mut self.locs.last_mut().unwrap(), loc)
    }
}


#[derive(Debug, Copy, Clone)]
pub struct VarLoc {
    pub loc_type: LocType,
    pub slot: usize,
    pub scope: usize,
}
impl VarLoc {
    pub fn new(loc_type: LocType, slot: usize, scope: usize) -> Self {
        Self {
            loc_type,
            slot,
            scope
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum LocType {
    Global,
    Local,
}