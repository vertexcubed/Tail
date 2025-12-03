use std::collections::HashMap;
use crate::ast::{BinOp, Block, Expr, ExprKind, FuncBlock, Identifier, Literal, Stmt, StmtKind, UOp};
use crate::ast::visit::AstVisitor;
use crate::compile::{CodeChunkBuilder, CompileError, RawConstantEntry, RawConstantKind};
use crate::vm::{CodeChunk, Instruction};

pub struct Compiler {
    // we hash by the constant entry, returning the index that's then stuck into the bytecode
    constant_pool: HashMap<RawConstantEntry, usize>,
    pub current_chunk: CodeChunkBuilder,

    frame_variable_slots: HashMap<Identifier, VarLoc>,
    // are we in the global scope right now?
    is_global: bool,
    next_pool_slot: usize,
    next_var_slot: usize,
    scope_level: usize,
}
impl Compiler {
    pub fn new() -> Self {
        Self {
            constant_pool: HashMap::new(),
            current_chunk: CodeChunkBuilder::new(),

            frame_variable_slots: HashMap::new(),
            is_global: true,
            next_pool_slot: 0,
            next_var_slot: 0,
            scope_level: 0
        }
    }


    pub fn reserve_var_slot(&mut self) -> usize {
        let slot = self.next_var_slot;
        self.next_var_slot += 1;
        slot
    }

    pub fn reserve_pool_slot(&mut self) -> usize {
        let slot = self.next_pool_slot;
        self.next_pool_slot += 1;
        slot
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
            self.current_chunk.push_instr(i);
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
                // if self.is_global {
                //     todo!("'Global' store/load NYI")
                // }
                // visit the body expr, which pushes its result onto the stack
                self.visit_expr(body)?;
                // then, we store it into the next available slot.
                // cases:
                //      does not exist: just add a new value, easy
                //      current scope < top scope: uhhh come back to
                //      current scope = top scope: modify the slot to next available slot
                //      current scope > top scope: push a new scope, store in next available slot
                let slot = self.reserve_var_slot();
                if let Some(loc) = self.frame_variable_slots.get_mut(id) {
                    let (_, top_scope) = loc.peek();
                    if self.scope_level < top_scope {
                        todo!()
                    }
                    else if self.scope_level == top_scope {
                        loc.replace_top(slot, top_scope);
                    }
                    else {
                        loc.push(slot, self.scope_level);
                    }
                }
                else {
                    // easy: add a new value
                    self.frame_variable_slots.insert(id.clone(), VarLoc::new(slot, self.scope_level));
                }

                // make the instruction
                let instr = match slot {
                    0 => Instruction::Store0,
                    1 => Instruction::Store1,
                    2 => Instruction::Store2,
                    3 => Instruction::Store3,
                    _ => Instruction::Store(slot)
                };
                // push it
                self.current_chunk.push_instr(instr);
                Ok(())

            }
            StmtKind::Assign(place, body) => {
                todo!()
            }
            // expressions are easy: just compile, then void the output (pop it off the stack)
            StmtKind::Expr(e) => {
                self.visit_expr(e)?;
                self.current_chunk.push_instr(Instruction::Pop);
                Ok(())
            }
            // returns are also easy: just compile the expr then push a "ret" instruction
            StmtKind::Ret(body) => {
                if let Some(e) = body {
                    self.visit_expr(e)?;
                }
                self.current_chunk.push_instr(Instruction::Ret);
                Ok(())
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        match &expr.kind {
            ExprKind::Lit(lit) => self.visit_literal(lit),
            ExprKind::Ident(id) => {
                let loc = self.frame_variable_slots.get_mut(id).unwrap();

                // TODO: global load/store instr



                // if current scope < scope, pop it off until its not. we're not in that scope anymore.
                while self.scope_level < loc.peek().1 {
                    loc.pop();
                }
                let (slot, scope) = loc.peek();
                if self.scope_level > scope {
                    // TODO: upvalues
                }
                else {
                    let instr = match slot {
                        0 => Instruction::Load0,
                        1 => Instruction::Load1,
                        2 => Instruction::Load2,
                        3 => Instruction::Load3,
                        _ => Instruction::Load(slot)
                    };
                    self.current_chunk.push_instr(instr);
                }

                Ok(())
            },
            ExprKind::UnaryOp(uop, operand) => {
                self.visit_expr(operand)?;
                let instr = match uop {
                    // boolean not actually behaves identically to Eq -> if 0, push 1. if not 0, push 0.
                    UOp::Not => Instruction::Eq,
                    // TODO: operator overloading
                    UOp::Neg => Instruction::INeg,
                };
                self.current_chunk.push_instr(instr);
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
                let cond_jump_ip = self.current_chunk.current_ip();
                self.current_chunk.push_instr(Instruction::JEq(0)); // temp address

                // compile then branch
                self.visit_block(then_branch)?;
                // also used later to write the correct jump address
                let finish_jump_ip = self.current_chunk.current_ip();
                self.current_chunk.push_instr(Instruction::Jump(0)); // temp address
                // compile else branch
                self.visit_block(else_branch)?;

                let end_if_ip = self.current_chunk.current_ip();

                // finish_jump_ip + 1 = start of else branch
                *self.current_chunk.get_instr_mut(cond_jump_ip) = Instruction::JEq((finish_jump_ip + 1) as u16);
                // end_if_ip = end of the if statement
                *self.current_chunk.get_instr_mut(finish_jump_ip) = Instruction::Jump(end_if_ip as u16);

                Ok(())
            },
            ExprKind::Closure(_, _, _) => todo!(),
            ExprKind::Call(_, _) => todo!(),
            ExprKind::Ref(e) => {
                self.visit_expr(e)?;
                self.current_chunk.push_instr(Instruction::Alloc);
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
        self.current_chunk.push_instr(instr);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct VarLoc {
    // slots are (slot#, scope)
    slots: Vec<(usize, usize)>
}
impl VarLoc {
    pub fn new(slot: usize, scope: usize) -> Self {
        Self {
            slots: vec![(slot, scope)],
        }
    }

    pub fn peek(&self) -> (usize, usize) {
        self.slots.last().unwrap().clone()
    }

    pub fn pop(&mut self) -> (usize, usize) {
        self.slots.pop().unwrap()
    }

    pub fn push(&mut self, slot: usize, scope: usize) {
        self.slots.push((slot, scope));
    }

    pub fn replace_top(&mut self, slot: usize, scope: usize) {
        *self.slots.last_mut().unwrap() = (slot, scope);
    }
}