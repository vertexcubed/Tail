use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use crate::ast::{BinOp, Block, Expr, ExprKind, FuncBlock, Identifier, Literal, Stmt, StmtKind, UOp};
use crate::ast::visit::AstVisitor;
use crate::compile::{CodeChunkBuilder, CompileError, RawConstantEntry, RawConstantKind};
use crate::vm;
use crate::vm::{CodeChunk, Instruction};


pub struct Compiler {
    constant_pool: HashMap<RawConstantEntry, usize>,
    next_pool_slot: usize,
    pub current_frame: FrameCompiler,
    frame_stack: Vec<FrameCompiler>,
    next_global_slot: usize,
}
impl Compiler {
    pub fn new() -> Self {
        Self {
            current_frame: FrameCompiler::new(true),
            constant_pool: HashMap::new(),
            next_pool_slot: 0,
            next_global_slot: 0,
            frame_stack: Vec::new(),
        }
    }

    pub fn reserve_pool_slot(&mut self) -> usize {
        let slot = self.next_pool_slot;
        self.next_pool_slot += 1;
        slot
    }
    pub fn reserve_global_slot(&mut self) -> usize {
        let slot = self.next_global_slot;
        self.next_global_slot += 1;
        slot
    }

    pub fn resolve_upvalue(&mut self, id: &Identifier) -> VarLoc {
        self._resolve_upvalue(self.frame_stack.len() - 1, id)
    }

    fn _resolve_upvalue(&mut self, index: usize, id: &Identifier) -> VarLoc {
        let frame = &self.frame_stack[index];
        // if this frame had it: yippee!! just return whatever this loc is
        if let Some(stack) = frame.frame_variable_slots.get(id) {
            return stack.peek();
        }
        // else this loc wasn't in this one either. let's recurse...
        if index == 0 {
            panic!("Identifier {} not present at all! Should not happen", id)
        }
        // previous upvalue.
        let prev = self._resolve_upvalue(index - 1, id);
        // if prev is a global, we just kinda. pass it down and do nothing.
        if let VarLoc::Global {..} = prev {
            return prev;
        }
        // reserve an upvalue and return it
        // can't use frame because of multiple mutable refs
        self.frame_stack[index].reserve_upvalue(prev)
    }


    pub fn temp_map_constants(&self) -> Vec<vm::ConstantPoolEntry> {
        let mut out = vec![];
        let mut entries = self.constant_pool.iter().collect::<Vec<_>>();
        entries.sort_by(|a, b| a.1.cmp(&b.1));
        for (k, v) in entries.into_iter() {
            assert_eq!(*v, out.len());
            out.push(k.clone().into())
        }
        out
    }
}





// compiles a single stack frame
pub struct FrameCompiler {
    pub chunk: CodeChunkBuilder,
    // if true, this is the root frame compiler. scope 0 = Global
    is_root: bool,
    frame_variable_slots: HashMap<Identifier, LocStack>,
    upvalues: Vec<VarLoc>,
    next_var_slot: usize,
    scope_level: usize,
}
impl FrameCompiler {
    pub fn new(is_root: bool) -> Self {
        Self {
            chunk: CodeChunkBuilder::new(),

            frame_variable_slots: HashMap::new(),
            is_root,
            next_var_slot: 0,
            upvalues: Vec::new(),
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
            while loc.len() > 0 {
                match loc.peek() {
                    VarLoc::Local { slot: _, scope } => {
                        if self.scope_level < scope {
                            loc.pop();
                        }
                    }
                    _ => break
                }
            }
            if loc.len() == 0 {
                self.frame_variable_slots.remove(id);
            }
        }
    }

    // guaranteed to never be a Global
    pub fn reserve_upvalue(&mut self, loc: VarLoc) -> VarLoc {
        if let VarLoc::Global {..} = loc {
            panic!("Can't reserve an upvalue for a global loc. Should never happen")
        }
        let index = self.upvalues.len();
        self.upvalues.push(loc);
        VarLoc::UpValue { slot: index }
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

impl AstVisitor for Compiler {
    type ExprResult = Result<(), CompileError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::ExprResult {
        match &stmt.kind {
            // lets are pretty straightforward: we push the value then store, though now we need to keep track of slots.
            StmtKind::Let(id, body) => {
                // visit the body expr, which pushes its result onto the stack
                self.visit_expr(body)?;
                // then, we store it into the next available slot.
                // cases:
                //      does not exist: just add a new value, easy. then we can just do the shit ig
                //      current scope < top scope: repeatedly pop until current >= top_scope
                //      current scope = top scope: modify the slot to next available slot
                //      current scope > top scope: push a new scope, store in next available slot
                let current_scope = self.current_frame.scope_level;
                let new_loc = if current_scope == 0 && self.current_frame.is_root {
                    VarLoc::Global { slot: self.reserve_global_slot() }
                } else {
                    VarLoc::Local { slot: self.current_frame.reserve_var_slot(), scope: current_scope }
                };
                // if local not present, calculate it
                if !self.current_frame.frame_variable_slots.contains_key(id) {
                    self.current_frame.frame_variable_slots.insert(id.clone(), LocStack::new(new_loc));
                }
                let loc_stack = self.current_frame.frame_variable_slots.get_mut(id).unwrap();
                match loc_stack.peek() {
                    // if current scope == top scope, just override instead of pushing a new thing
                    VarLoc::Local { slot: _, scope } if current_scope == scope => {
                        loc_stack.replace_top(new_loc);
                    }
                    // if not just push smth ig lol
                    _ => {
                        loc_stack.push(new_loc);
                    }
                }
                let instr = match new_loc {
                    VarLoc::Local { slot: 0, scope: _ } => Instruction::Store0,
                    VarLoc::Local { slot: 1, scope: _ } => Instruction::Store1,
                    VarLoc::Local { slot: 2, scope: _ } => Instruction::Store2,
                    VarLoc::Local { slot: 3, scope: _ } => Instruction::Store3,
                    VarLoc::Local { slot, scope: _ } => Instruction::Store(slot),
                    VarLoc::Global { slot: 0 } => Instruction::GStore0,
                    VarLoc::Global { slot: 1 } => Instruction::GStore1,
                    VarLoc::Global { slot: 2 } => Instruction::GStore2,
                    VarLoc::Global { slot: 3 } => Instruction::GStore3,
                    VarLoc::Global { slot } => Instruction::GStore(slot),
                    VarLoc::UpValue {..} => unreachable!("Can't let bind an upvalue"),
                };

                // push it
                self.current_frame.chunk.push_instr(instr);
                Ok(())

            }
            StmtKind::Assign(place, body) => {
                todo!()
            }
            // expressions are easy: just compile, then void the output (pop it off the stack)
            StmtKind::Expr(e) => {
                self.visit_expr(e)?;
                self.current_frame.chunk.push_instr(Instruction::Pop);
                Ok(())
            }
            // returns are also easy: just compile the expr then push a "ret" instruction
            StmtKind::Ret(body) => {
                if let Some(e) = body {
                    self.visit_expr(e)?;
                }
                self.current_frame.chunk.push_instr(Instruction::Ret);
                Ok(())
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        match &expr.kind {
            ExprKind::Ident(id) => {
                // can't find identifier - resolve either a global or upvalue and store it.
                if self.current_frame.frame_variable_slots.get_mut(id).is_none() {

                    // could not find variable in the current scope - this is either global, or needs to be captured.
                    let loc = self.resolve_upvalue(id);
                    match loc {
                        VarLoc::Global { .. } => {
                            // we found a global. bind to environment for convenience and then compile instr
                            self.current_frame.frame_variable_slots.insert(id.clone(), LocStack::new(loc));
                        }
                        _ => {
                            let upvalue = self.current_frame.reserve_upvalue(loc);
                            // bind the upvalue to the environment for later
                            self.current_frame.frame_variable_slots.insert(id.clone(), LocStack::new(upvalue));

                        }
                    }
                };
                let loc = self.current_frame.frame_variable_slots.get(id).unwrap().peek();
                let instr = match loc {
                    VarLoc::Local { slot: 0, scope: _ } => Instruction::Load0,
                    VarLoc::Local { slot: 1, scope: _ } => Instruction::Load1,
                    VarLoc::Local { slot: 2, scope: _ } => Instruction::Load2,
                    VarLoc::Local { slot: 3, scope: _ } => Instruction::Load3,
                    VarLoc::Local { slot, scope: _ } => Instruction::Load(slot),
                    VarLoc::Global { slot: 0 } => Instruction::GLoad0,
                    VarLoc::Global { slot: 1 } => Instruction::GLoad1,
                    VarLoc::Global { slot: 2 } => Instruction::GLoad2,
                    VarLoc::Global { slot: 3 } => Instruction::GLoad3,
                    VarLoc::Global { slot } => Instruction::GLoad(slot),
                    VarLoc::UpValue { slot } => Instruction::UpLoad(slot),
                };
                self.current_frame.chunk.push_instr(instr);
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
                self.current_frame.chunk.push_instr(instr);
                Ok(())
            },

            ExprKind::BinaryOp(bop, left, right) => {
                // TODO: Update calling convention to be left -> right not right -> left (unintuitive)
                self.visit_expr(right)?;
                self.visit_expr(left)?;
                self.current_frame.push_bop_instr(bop)
            },
            ExprKind::If(cond, then_branch, else_branch) => {
                self.visit_expr(cond)?;
                // this is used to later write the correct jump address
                let cond_jump_ip = self.current_frame.chunk.current_ip();
                self.current_frame.chunk.push_instr(Instruction::JEq(0)); // temp address

                // compile then branch
                self.visit_block(then_branch)?;
                // also used later to write the correct jump address
                let finish_jump_ip = self.current_frame.chunk.current_ip();
                self.current_frame.chunk.push_instr(Instruction::Jump(0)); // temp address
                // compile else branch
                self.visit_block(else_branch)?;

                let end_if_ip = self.current_frame.chunk.current_ip();

                // finish_jump_ip + 1 = start of else branch
                *self.current_frame.chunk.get_instr_mut(cond_jump_ip) = Instruction::JEq((finish_jump_ip + 1) as u16);
                // end_if_ip = end of the if statement
                *self.current_frame.chunk.get_instr_mut(finish_jump_ip) = Instruction::Jump(end_if_ip as u16);

                Ok(())
            },
            ExprKind::Closure(name, args, body) => {


                Ok(())
            }
            ExprKind::Call(_, _) => todo!(),
            ExprKind::Ref(e) => {
                self.visit_expr(e)?;
                self.current_frame.chunk.push_instr(Instruction::Alloc);
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
        self.current_frame.scope_level += 1;
        for i in 0..block.stmts.len() {
            let stmt = &block.stmts[i];
            // if i is the last stmt and the stmt is an expr: just compile the expr without the pop
            // that way, the last value will still be on the stack - normally, expr; adds a pop expression so.
            if i == block.stmts.len() - 1 {
                if let StmtKind::Expr(e) = &stmt.kind {
                    self.visit_expr(e)?;
                }
                else {
                    // just visit normally and push unit onto the stack
                    self.visit_stmt(stmt)?;
                    self.current_frame.chunk.push_instr(Instruction::IPush0);
                }
            }
            else {
                self.visit_stmt(stmt)?;
            }
        }

        self.current_frame.scope_level -= 1;
        let keys = self.current_frame.frame_variable_slots.keys().cloned().collect::<Vec<_>>();
        for k in keys.iter() {
            self.current_frame.trim_locs(k);
        }
        Ok(())
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

                // if entry doesn't exist, add it.
                if !self.constant_pool.contains_key(&entry) {
                    let slot = self.reserve_pool_slot();
                    self.constant_pool.insert(entry.clone(), slot);
                }

                Instruction::Ldc(self.constant_pool.get(&entry).unwrap().clone())
            }
            Literal::Float(f) => todo!(),
            Literal::Char(c) => todo!(),
            Literal::Str(s) => todo!(),
            Literal::Bool(true) => Instruction::IPush1,
            Literal::Bool(false) => Instruction::IPush0,
        };
        self.current_frame.chunk.push_instr(instr);
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
pub enum VarLoc {
    Local { slot: usize, scope: usize },
    Global { slot: usize },
    UpValue { slot: usize }
}