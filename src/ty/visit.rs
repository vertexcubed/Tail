use crate::ast::visit::AstVisitor;
use crate::ast::{BinOp, Block, Expr, ExprKind, FuncBlock, Identifier, Literal, NodeId, Stmt, StmtKind, UOp};
use crate::ty::{Ty, TyCtxt, TyError};
use std::collections::HashMap;

#[derive(Debug)]
pub struct TypeVisitor {
    pub ctxt: TyCtxt,
    node_types: HashMap<NodeId, Ty>,
}
impl TypeVisitor {

    pub fn new() -> Self {
        Self {
            ctxt: TyCtxt::new(),
            node_types: HashMap::new(),
        }
    }
    fn bop_types(&self, op: BinOp) -> (Ty, Ty) {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod =>
                (self.ctxt.common_types.int.clone(), self.ctxt.common_types.int.clone()),
            BinOp::And | BinOp::Or =>
                (self.ctxt.common_types.bool.clone(), self.ctxt.common_types.bool.clone()),
            BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::NEq =>
                (self.ctxt.common_types.int.clone(), self.ctxt.common_types.bool.clone())
        }
    }

    pub fn get_type_of_expr(&self, node: NodeId) -> Option<&Ty> {
        self.node_types.get(&node)
    }

    pub fn set_type_of_expr(&mut self, node: NodeId, ty: Ty) {
        self.node_types.insert(node, ty);
    }
}
impl AstVisitor for TypeVisitor {
    type ExprResult = Result<Ty, TyError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::ExprResult {
        match &stmt.kind {
            StmtKind::Let(iden, body) => {
                let body_ty = self.visit_expr(body)?;

                // TODO: let polymorphism
                self.ctxt.bind(iden.clone(), body_ty.clone());
                self.set_type_of_expr(stmt.id, self.ctxt.common_types.unit.clone());

                Ok(self.ctxt.common_types.unit.clone())
            }
            StmtKind::Assign(place, body) => {
                // TODO struct field lookup
                
                let ExprKind::Ident(iden) = &place.inner.kind else {
                    unreachable!("Parser should prevent this from happening")
                };


                // type of the identifier. if this is a ref, it may be ref int
                let derefed_ty = self.ctxt.new_type_var();
                let mut place_ty = derefed_ty.clone();
                for _ in 0..place.derefs {
                    place_ty = self.ctxt.new_ref_ty(place_ty);
                }
                let inner = self.visit_ident(iden)?;
                self.ctxt.unify(&inner, &place_ty)?;



                let body_ty = self.visit_expr(body)?;
                self.ctxt.unify(&derefed_ty, &body_ty)?;

                self.ctxt.bind(iden.clone(), body_ty.clone());
                self.set_type_of_expr(stmt.id, self.ctxt.common_types.unit.clone());

                Ok(self.ctxt.common_types.unit.clone())
            }
            StmtKind::Expr(e) => {
                let ty = self.visit_expr(e)?;
                self.set_type_of_expr(stmt.id, ty.clone());
                Ok(ty)
            }
            StmtKind::Ret(expr) => {
                let expr_ty = match expr {
                    Some(e) => self.visit_expr(e)?,
                    None => self.ctxt.common_types.unit.clone(),
                };
                self.ctxt.unify_ret(&Some(expr_ty))?;
                self.set_type_of_expr(stmt.id, self.ctxt.common_types.unit.clone());
                Ok(self.ctxt.common_types.unit.clone())
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        match &expr.kind {
            ExprKind::Lit(l) => {
                let lit_ty = self.visit_literal(l)?;
                self.set_type_of_expr(expr.id, lit_ty.clone());
                Ok(lit_ty)
            }
            ExprKind::Ident(id) => {
                let id_ty = self.visit_ident(id)?;
                self.set_type_of_expr(expr.id, id_ty.clone());
                Ok(id_ty)
            }
            ExprKind::UnaryOp(uop, e) => {
                match uop {
                    UOp::Not => {
                        let operand = self.visit_expr(e)?;
                        self.ctxt.unify(&operand, &self.ctxt.common_types.bool.clone())?;
                        self.set_type_of_expr(expr.id, operand.clone());
                        Ok(operand)
                    }
                    UOp::Neg => {
                        let operand = self.visit_expr(e)?;
                        self.ctxt.unify(&operand, &self.ctxt.common_types.int.clone())?;
                        self.set_type_of_expr(expr.id, operand.clone());
                        Ok(operand)
                    }
                }
            },
            ExprKind::BinaryOp(bop, left, right) => {
                let (arg, ret) = self.bop_types(*bop);
                let left_ty = self.visit_expr(left)?;
                let right_ty = self.visit_expr(right)?;
                self.ctxt.unify(&left_ty, &arg)?;
                self.ctxt.unify(&right_ty, &arg)?;
                self.set_type_of_expr(expr.id, ret.clone());
                Ok(ret)
            },
            ExprKind::If(cond, then_branch, else_branch) => {
                let cond_ty = self.visit_expr(cond)?;
                self.ctxt.unify(&cond_ty, &self.ctxt.common_types.bool.clone())?;
                self.ctxt.enter_scope();
                let then_ty = self.visit_block(then_branch)?;
                let then_ret_ty = self.ctxt.get_ret_ty();
                self.ctxt.exit_scope();
                self.ctxt.unify_ret(&then_ret_ty)?;
                self.ctxt.enter_scope();
                let else_ty = self.visit_block(else_branch)?;
                let else_ret_ty = self.ctxt.get_ret_ty();
                self.ctxt.exit_scope();
                self.ctxt.unify_ret(&else_ret_ty)?;
                self.ctxt.unify(&then_ty, &else_ty)?;
                self.set_type_of_expr(expr.id, then_ty.clone());
                Ok(then_ty)
            }
            ExprKind::Call(func, args) => {
                // first, type check the function
                let func_ty = self.visit_expr(func)?;
                // next, type check the args
                let arg_types = args.iter().map(|e| self.visit_expr(e)).collect::<Result<Vec<_>,_>>()?;
                // now, create a new type var for the return type, or "our" type
                let my_type = self.ctxt.new_type_var();
                // this is what we expect: a function of type (arg1, arg2, ..., argN) -> t
                let expected_ty = self.ctxt.new_function_ty(arg_types, my_type.clone());
                self.ctxt.unify(&func_ty, &expected_ty)?;
                self.set_type_of_expr(expr.id, my_type.clone());
                Ok(my_type)
            },
            ExprKind::Ref(value) => {
                let inner_ty = self.visit_expr(value)?;
                let ty = self.ctxt.new_ref_ty(inner_ty);
                self.set_type_of_expr(expr.id, ty.clone());
                Ok(ty)
            },
            ExprKind::Block(block) => {
                self.ctxt.enter_scope();
                let ty = self.visit_block(block)?;
                let block_ret = self.ctxt.get_ret_ty();
                self.ctxt.exit_scope();
                self.ctxt.unify_ret(&block_ret)?;
                Ok(ty)
            },
            ExprKind::Struct(_strukt) => todo!(),
            ExprKind::Closure(name, args, body) => {
                let arg_types = args.iter().map(|e| {
                    let ty = self.ctxt.new_type_var();
                    self.ctxt.bind(e.clone(), ty.clone());
                    ty
                }).collect::<Vec<_>>();

                if let Some(name) = name {
                    // if name is some, then this is a recursive closure. We bind the name preemptively and then typecheck the body
                    let expected_ret = self.ctxt.new_type_var();
                    let ret = self.ctxt.new_function_ty(arg_types, expected_ret.clone());
                    self.ctxt.enter_func_scope();
                    self.ctxt.bind(name.clone(), ret.clone());
                    let body_ty = self.visit_func_block(body)?;
                    self.ctxt.exit_func_scope();
                    self.ctxt.unify(&body_ty, &expected_ret)?;
                    self.set_type_of_expr(expr.id, ret.clone());
                    Ok(ret)
                }
                else {
                    self.ctxt.enter_func_scope();
                    let body_ty = self.visit_func_block(body)?;
                    self.ctxt.exit_func_scope();
                    let ret = self.ctxt.new_function_ty(arg_types, body_ty);
                    self.set_type_of_expr(expr.id, ret.clone());
                    Ok(ret)
                }
            }
            ExprKind::Place(place) => {
                // TODO: Struct lookups
                let first = self.ctxt.new_type_var();
                let mut ty = first.clone();
                for _ in 0..place.derefs {
                    ty = self.ctxt.new_ref_ty(ty);
                }
                let inner = self.visit_expr(&place.inner)?;
                self.ctxt.unify(&inner, &ty)?;
                self.set_type_of_expr(expr.id, first.clone());
                Ok(first)
            },
        }
    }
    fn visit_ident(&mut self, ident: &Identifier) -> Self::ExprResult {
        // instantiate
        self.ctxt.get_binding(ident)
    }

    fn visit_block(&mut self, block: &Block) -> Self::ExprResult {
        let mut ty = self.ctxt.common_types.unit.clone();
        for stmt in block.stmts.iter() {
            ty = self.visit_stmt(stmt)?;
        }
        Ok(ty)
    }

    // type of this is the type of the `return` stmt. If none, the type is ()
    fn visit_func_block(&mut self, block: &FuncBlock) -> Self::ExprResult {
        for stmt in block.stmts.iter() {
            self.visit_stmt(stmt)?;
        }
        Ok(self.ctxt.get_ret_ty().unwrap_or(self.ctxt.common_types.unit.clone()))
    }

    fn visit_literal(&mut self, literal: &Literal) -> Self::ExprResult {
        Ok(match literal {
            Literal::Int(_) => self.ctxt.common_types.int.clone(),
            Literal::Float(_) => self.ctxt.common_types.float.clone(),
            Literal::Char(_) => self.ctxt.common_types.char.clone(),
            Literal::Str(_) => panic!("Strings NYI"),
            Literal::Bool(_) => self.ctxt.common_types.bool.clone(),
            Literal::Unit => self.ctxt.common_types.unit.clone(),
        })
    }
}