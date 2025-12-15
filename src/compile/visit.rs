use crate::ast::visit::AstVisitor;
use crate::ast::{BinOp, Block, Expr, ExprKind, FuncBlock, Identifier, Literal, Stmt, StmtKind, UOp};
use crate::compile::{CodeChunkBuilder, CodeChunkIndex, CompileError, RawConstantEntry};
use crate::vm::def::{CaptureDef, FunctionDef};
use crate::vm::{CodeChunk, Instruction, SourceFile};
use std::collections::HashMap;
use crate::ty::{ExportedTypes, TyKind};

pub struct Compiler {
    exported_types: ExportedTypes,
    constant_pool: HashMap<RawConstantEntry, usize>,
    next_pool_slot: usize,
    frame_stack: Vec<FrameCompiler>,
    finished_code: HashMap<usize, CodeChunk>,
    next_global_slot: usize,
    next_func_index: usize,
}
impl Compiler {
    pub fn new(exported_types: ExportedTypes) -> Self {
        println!("{:?}", exported_types);
        Self {
            exported_types,
            constant_pool: HashMap::new(),
            next_pool_slot: 0,
            next_global_slot: 0,
            frame_stack: vec![FrameCompiler::root()],
            next_func_index: 0,
            finished_code: HashMap::new(),
        }
    }


    /// Gets the index for this constant in the raw constant pool. If it's not in the pool, add it.
    pub fn get_or_insert_constant(&mut self, entry: RawConstantEntry) -> usize {
        match self.constant_pool.get(&entry) {
            Some(e) => *e,
            None => {
                let slot = self.reserve_pool_slot();
                self.constant_pool.insert(entry, slot);
                slot
            }
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

    pub fn reserve_func_index(&mut self) -> usize {
        let index = self.next_func_index;
        self.next_func_index += 1;
        index
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
        self.frame_stack[index].add_upvalue(prev)
    }

    pub fn frame(&mut self) -> &mut FrameCompiler {
        self.frame_stack.last_mut().unwrap()
    }

    pub fn push_frame(&mut self, index: usize) {
        self.frame_stack.push(FrameCompiler::new(index));
    }
    pub fn pop_frame(&mut self) -> FrameCompiler {
        // sanity check: can't pop the root
        assert!(self.frame_stack.len() > 1);
        self.frame_stack.pop().unwrap()
    }

    pub fn build(self) -> SourceFile {

        // build constant pool
        let mut constant_pool = Vec::new();
        let mut entries = self.constant_pool.into_iter().collect::<Vec<_>>();
        entries.sort_by(|a, b| a.1.cmp(&b.1));
        for (k, v) in entries.into_iter() {
            assert_eq!(v, constant_pool.len());
            constant_pool.push(k.into())
        };

        // build functions
        let mut functions = Vec::new();
        let mut entries = self.finished_code.into_iter().collect::<Vec<_>>();
        entries.sort_by(|a, b| a.0.cmp(&b.0));
        for (k, v) in entries.into_iter() {
            assert_eq!(k, functions.len());
            functions.push(v);
        }

        // build main
        let mut main = self.frame_stack;
        assert_eq!(main.len(), 1);
        let main = main.pop().unwrap().chunk.build();


        SourceFile {
            constant_pool,
            main_code: main,
            functions
        }
    }
}





// compiles a single stack frame
pub struct FrameCompiler {
    pub chunk: CodeChunkBuilder,
    frame_variable_slots: HashMap<Identifier, LocStack>,
    upvalues: Vec<VarLoc>,
    next_var_slot: usize,
    scope_level: usize,
}
#[allow(dead_code)]
impl FrameCompiler {

    // if true, this is the root frame compiler. scope 0 = Global
    pub fn root() -> Self {
        Self {
            chunk: CodeChunkBuilder::root(),
            frame_variable_slots: HashMap::new(),
            next_var_slot: 0,
            upvalues: Vec::new(),
            scope_level: 0
        }
    }
    pub fn new(func_index: usize) -> Self {
        Self {
            chunk: CodeChunkBuilder::new(func_index),
            frame_variable_slots: HashMap::new(),
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
                        else {
                            break
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
    pub fn add_upvalue(&mut self, loc: VarLoc) -> VarLoc {
        if let VarLoc::Global {..} = loc {
            panic!("Can't reserve an upvalue for a global loc. Should never happen")
        }
        let index = self.upvalues.len();
        self.upvalues.push(loc);
        VarLoc::UpValue { slot: index }
    }

    pub fn push_instr(&mut self, instruction: Instruction) -> usize {
        let out = self.chunk.current_ip();
        self.chunk.push_instr(instruction);
        out
    }

    pub fn is_root(&self) -> bool {
        self.chunk.is_root()
    }

    pub fn func_index(&self) -> CodeChunkIndex {
        self.chunk.func_index()
    }


    fn _push_bop_instr(&mut self, bop: &BinOp) -> Result<(), CompileError> {
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
                let expr_ty = self.exported_types.get_node_type(&body.id);
                if expr_ty.is_some() && matches!(expr_ty.unwrap().kind, TyKind::Unit) {
                    self.frame().push_instr(Instruction::IPush0);
                }
                // then, we store it into the next available slot.
                // cases:
                //      does not exist: just add a new value, easy. then we can just do the shit ig
                //      current scope < top scope: repeatedly pop until current >= top_scope
                //      current scope = top scope: modify the slot to next available slot
                //      current scope > top scope: push a new scope, store in next available slot
                let current_scope = self.frame().scope_level;
                let new_loc = if current_scope == 0 && self.frame().is_root() {
                    VarLoc::Global { slot: self.reserve_global_slot() }
                } else {
                    VarLoc::Local { slot: self.frame().reserve_var_slot(), scope: current_scope }
                };
                // if local not present, calculate it
                if !self.frame().frame_variable_slots.contains_key(id) {
                    self.frame().frame_variable_slots.insert(id.clone(), LocStack::new(new_loc));
                }
                let loc_stack = self.frame().frame_variable_slots.get_mut(id).unwrap();
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
                self.frame().push_instr(instr);
                Ok(())

            }
            StmtKind::Assign(place, body) => {
                let ExprKind::Ident(id) = &place.inner.kind else {
                    panic!("This should never happen!")
                };



                // can't find identifier - resolve either a global or upvalue and store it.
                if self.frame().frame_variable_slots.get_mut(id).is_none() {
                    // could not find variable in the current scope - this is either global, or needs to be captured.
                    let loc = self.resolve_upvalue(id);
                    match loc {
                        VarLoc::Global { .. } => {
                            // we found a global. bind to environment for convenience and then compile instr
                            self.frame().frame_variable_slots.insert(id.clone(), LocStack::new(loc));
                        }
                        _ => {
                            // bind the upvalue to the environment for later
                            self.frame().frame_variable_slots.insert(id.clone(), LocStack::new(loc));

                        }
                    }
                };
                let loc = self.frame().frame_variable_slots.get(id).unwrap().peek();

                // load only used if derefs
                let (load, store) = match loc {
                    VarLoc::Local { slot: 0, scope: _ } => (Instruction::Load0, Instruction::Store0),
                    VarLoc::Local { slot: 1, scope: _ } => (Instruction::Load1, Instruction::Store1),
                    VarLoc::Local { slot: 2, scope: _ } => (Instruction::Load2, Instruction::Store2),
                    VarLoc::Local { slot: 3, scope: _ } => (Instruction::Load3, Instruction::Store3),
                    VarLoc::Local { slot, scope: _ } => (Instruction::Load(slot), Instruction::Store(slot)),
                    VarLoc::Global { slot: 0 } => (Instruction::GLoad0, Instruction::GStore0),
                    VarLoc::Global { slot: 1 } => (Instruction::GLoad1, Instruction::GStore1),
                    VarLoc::Global { slot: 2 } => (Instruction::GLoad2, Instruction::GStore2),
                    VarLoc::Global { slot: 3 } => (Instruction::GLoad3, Instruction::GStore3),
                    VarLoc::Global { slot } => (Instruction::GLoad(slot), Instruction::GStore(slot)),
                    VarLoc::UpValue { slot } => (Instruction::UpLoad(slot), Instruction::UpStore(slot)),
                };

                if place.derefs > 0 {
                    self.frame().push_instr(load);
                    for _ in 0..place.derefs - 1 {
                        self.frame().push_instr(Instruction::Deref);
                    }
                    self.visit_expr(body)?;
                    let expr_ty = self.exported_types.get_node_type(&body.id);
                    if expr_ty.is_some() && matches!(expr_ty.unwrap().kind, TyKind::Unit) {
                        self.frame().push_instr(Instruction::IPush0);
                    }
                    self.frame().push_instr(Instruction::Write);
                }
                else {
                    self.visit_expr(body)?;
                    let expr_ty = self.exported_types.get_node_type(&body.id);
                    if expr_ty.is_some() && matches!(expr_ty.unwrap().kind, TyKind::Unit) {
                        self.frame().push_instr(Instruction::IPush0);
                    }
                    self.frame().push_instr(store);
                }

                Ok(())

            }
            // expressions are easy: just compile, then void the output (pop it off the stack)
            StmtKind::Expr(e) => {
                self.visit_expr(e)?;

                // only pop if the value of expr is not unit - unit values wont be pushed
                let should_pop = {
                    let expr_ty = self.exported_types.get_node_type(&e.id);
                    expr_ty.is_none() || !matches!(expr_ty.unwrap().kind, TyKind::Unit)
                };
                if should_pop {
                    self.frame().push_instr(Instruction::Pop);
                }

                Ok(())
            }
            // returns are also easy: just compile the expr then push a "ret" instruction
            StmtKind::Ret(body) => {
                if let Some(e) = body {
                    self.visit_expr(e)?;
                }
                // else {
                //     // Push Unit
                //     self.frame().push_instr(Instruction::IPush0);
                // }
                self.frame().push_instr(Instruction::Ret);
                Ok(())
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        match &expr.kind {
            ExprKind::Ident(id) => {

                let expr_ty = self.exported_types.get_node_type(&expr.id);
                // if we're unit, we skip loading entirely.
                if expr_ty.is_some() && matches!(expr_ty.unwrap().kind, TyKind::Unit) {
                    return Ok(())
                }



                // can't find identifier - resolve either a global or upvalue and store it.
                if self.frame().frame_variable_slots.get_mut(id).is_none() {

                    // could not find variable in the current scope - this is either global, or needs to be captured.
                    let loc = self.resolve_upvalue(id);
                    match loc {
                        VarLoc::Global { .. } => {
                            // we found a global. bind to environment for convenience and then compile instr
                            self.frame().frame_variable_slots.insert(id.clone(), LocStack::new(loc));
                        }
                        _ => {
                            // bind the upvalue to the environment for later
                            self.frame().frame_variable_slots.insert(id.clone(), LocStack::new(loc));

                        }
                    }
                };
                let loc = self.frame().frame_variable_slots.get(id).unwrap().peek();
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
                self.frame().push_instr(instr);
                Ok(())
            },
            ExprKind::Lit(lit) => self.visit_literal(lit),
            ExprKind::UnaryOp(uop, operand) => {
                // TODO: unary unit operators? No lol
                self.visit_expr(operand)?;
                let instr = match uop {
                    // boolean not actually behaves identically to Eq -> if 0, push 1. if not 0, push 0.
                    UOp::Not => Instruction::Eq,
                    // TODO: operator overloading
                    UOp::Neg => Instruction::INeg,
                };
                self.frame().push_instr(instr);
                Ok(())
            },

            ExprKind::BinaryOp(bop, left, right) => {
                // TODO: binary operators on unit?
                // TODO: Update calling convention to be left -> right not right -> left (unintuitive)
                self.visit_expr(right)?;
                self.visit_expr(left)?;
                self.frame()._push_bop_instr(bop)
            },
            ExprKind::If(cond, then_branch, else_branch) => {
                // this is always a bool - no need to unit check
                self.visit_expr(cond)?;
                // this is used to later write the correct jump address
                let cond_jump_ip = self.frame().push_instr(Instruction::JEq(0)); // temp address

                // compile then branch
                self.visit_block(then_branch)?;
                // also used later to write the correct jump address
                let finish_jump_ip = self.frame().push_instr(Instruction::Jump(0)); // temp address
                // compile else branch
                self.visit_block(else_branch)?;

                let end_if_ip = self.frame().chunk.current_ip();

                // finish_jump_ip + 1 = start of else branch
                *self.frame().chunk.get_instr_mut(cond_jump_ip) = Instruction::JEq((finish_jump_ip + 1) as u16);
                // end_if_ip = end of the if statement
                *self.frame().chunk.get_instr_mut(finish_jump_ip) = Instruction::Jump(end_if_ip as u16);

                Ok(())
            },
            ExprKind::Closure(name, args, body) => {

                let next_index = self.reserve_func_index();
                // make our function def and stick it in the constant pool
                let def = RawConstantEntry::Function(FunctionDef {
                    arity: args.len() as u8,
                    code_chunk: next_index,
                });
                // grab the constant index
                let constant_index = self.get_or_insert_constant(def);

                // get the ip of the ClosPush instr.
                // We need to late bind the upvalues so we push an empty set of upvalues
                let clos_push_ip = self.frame().push_instr(Instruction::ClosPush(constant_index, vec![]));

                // push a new frame compiler and compile the function body
                // We also bind slots for self (if present) and args.
                self.push_frame(next_index);
                if let Some(name) = name {
                    self.frame().frame_variable_slots.insert(name.clone(), LocStack::new(VarLoc::Local { slot: 0, scope: 0}));
                }
                for (i, a) in args.iter().enumerate() {
                    self.frame().frame_variable_slots.insert(a.clone(), LocStack::new(VarLoc::Local { slot: i + 1, scope: 0 }));
                }
                // now we can finally compile the function block
                self.visit_func_block(body)?;
                // we've finished the body - let's pop off this frame compiler
                // needs to be mut cuz build() swaps out the CodeChunkBuilder idk if theres a good way to do get around this
                let mut old_frame = self.pop_frame();

                // quick sanity check
                debug_assert!(!old_frame.is_root());

                let mut captured = Vec::new();
                for up in old_frame.upvalues.iter() {
                    match up {
                        // we no longer care about scope info
                        VarLoc::Local { slot, scope: _ } => captured.push(CaptureDef::local(*slot as u8)),
                        VarLoc::UpValue { slot } => captured.push(CaptureDef::upvalue(*slot as u8)),
                        // skip on globals - we don't need to capture them so.
                        VarLoc::Global { .. } => {}
                    }
                }


                // top frame now corresponds to the right frame :)
                let Instruction::ClosPush(_, vec) = self.frame().chunk.get_instr_mut(clos_push_ip) else {
                    panic!("Malformed code chunk! Should never happen")
                };
                // swap out
                let _ = std::mem::replace(vec, captured);

                // finally, build the code chunk and stick it in our finished chunks
                self.finished_code.insert(next_index, old_frame.chunk.build());

                // and we're done :)
                Ok(())
            }
            ExprKind::Call(func, args) => {
                //TODO: update calling convention from left -> right

                for a in args.iter().rev() {
                    self.visit_expr(a)?;
                    let expr_ty = self.exported_types.get_node_type(&a.id);
                    if expr_ty.is_some() && matches!(expr_ty.unwrap().kind, TyKind::Unit) {
                        self.frame().push_instr(Instruction::IPush0);
                    }
                }
                // guaranteed to be a function, no need to unit check
                self.visit_expr(func)?;
                self.frame().push_instr(Instruction::Call);

                Ok(())
            },
            ExprKind::Ref(e) => {
                self.visit_expr(e)?;
                let expr_ty = self.exported_types.get_node_type(&e.id);
                if expr_ty.is_some() && matches!(expr_ty.unwrap().kind, TyKind::Unit) {
                    self.frame().push_instr(Instruction::IPush0);
                }
                self.frame().push_instr(Instruction::Alloc);
                Ok(())
            },
            ExprKind::Block(block) => {
                self.visit_block(block)
            },
            ExprKind::Struct(_) => todo!(),
            ExprKind::Place(p) => {

                // we don't have to do unit checks here - this expressiion will always either be a variable name or an expr of type ref 'a
                self.visit_expr(p.inner.as_ref())?;
                for _ in 0..p.derefs {
                    self.frame().push_instr(Instruction::Deref);
                }
                Ok(())
            },
        }
    }

    fn visit_ident(&mut self, _: &Identifier) -> Self::ExprResult {
        unreachable!("Unused")
    }

    fn visit_block(&mut self, block: &Block) -> Self::ExprResult {
        self.frame().scope_level += 1;


        // if block is empty: push Unit and thats it.
        if block.stmts.is_empty() {
            // self.frame().push_instr(Instruction::IPush0);
            // be sure to fix scope level!
            self.frame().scope_level -= 1;
            return Ok(());
        }
        for (i, stmt) in block.stmts.iter().enumerate() {
            // if i is the last stmt and the stmt is an expr: just compile the expr without the pop
            // that way, the last value will still be on the stack - normally, expr; adds a pop expression so.
            if i == block.stmts.len() - 1 {
                if let StmtKind::Expr(e) = &stmt.kind {
                    self.visit_expr(e)?;
                }
                else {
                    // just visit normally and push unit onto the stack
                    self.visit_stmt(stmt)?;
                    // self.frame().push_instr(Instruction::IPush0);
                }
            }
            else {
                self.visit_stmt(stmt)?;
            }
        }

        self.frame().scope_level -= 1;
        let keys = self.frame().frame_variable_slots.keys().cloned().collect::<Vec<_>>();
        for k in keys.iter() {
            self.frame().trim_locs(k);
        }
        Ok(())
    }

    // behaves largely the same as block but with a few key differences
    // 1: final expressions are NOT kept on the stack
    // 2: if the last instruction is not a Ret, we add a Ret to be safe. This should account for functions that return unit
    fn visit_func_block(&mut self, block: &FuncBlock) -> Self::ExprResult {
        for stmt in block.stmts.iter() {
            self.visit_stmt(stmt)?;
        }
        if self.frame().chunk.is_empty() || !matches!(self.frame().chunk.last(), Instruction::Ret) {
            // Push Unit
            // self.frame().push_instr(Instruction::IPush0);
            self.frame().push_instr(Instruction::Ret);
        }
        Ok(())
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
                let entry = RawConstantEntry::Int(*i);

                // Get the index of this constant
                let constant_index = self.get_or_insert_constant(entry);

                Instruction::Ldc(constant_index)
            }
            Literal::Float(_f) => todo!(),
            Literal::Char(_c) => todo!(),
            Literal::Str(_s) => todo!(),
            Literal::Bool(true) => Instruction::IPush1,
            Literal::Bool(false) => Instruction::IPush0,
            // we're going to trust whoever's reading this value pushes unit themself if needed
            Literal::Unit => return Ok(()),
        };
        self.frame().push_instr(instr);
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