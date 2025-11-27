use crate::ast::{Block, Expr, ExprKind, FuncBlock, Identifier, Literal, Stmt};
use crate::ast::visit::AstVisitor;
use crate::ty::{Ty, TyCtxt, TyError};

#[derive(Debug)]
pub struct TypeVisitor {
    pub ctxt: TyCtxt,
}
impl TypeVisitor {

}
impl AstVisitor for TypeVisitor {
    type Result = Result<Ty, TyError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result {
        todo!()
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
        match &expr.kind {
            ExprKind::Lit(l) => {
                let lit_ty = self.visit_literal(l)?;
                self.ctxt.set_type_of_expr(expr.id, lit_ty.clone());
                Ok(lit_ty)
            }
            ExprKind::Ident(id) => {
                let id_ty = self.visit_ident(id)?;
                self.ctxt.set_type_of_expr(expr.id, id_ty.clone());
                Ok(id_ty)
            }
            ExprKind::UnaryOp(uop, e) => todo!(),
            ExprKind::BinaryOp(bop, left, right) => todo!(),
            ExprKind::If(cond, then_branch, else_branch) => {
                let cond_ty = self.visit_expr(cond)?;
                self.ctxt.unify(&cond_ty, &self.ctxt.common_types.bool.clone())?;
                let then_ty = self.visit_block(then_branch)?;
                let else_ty = self.visit_block(else_branch)?;
                self.ctxt.unify(&then_ty, &else_ty)?;
                self.ctxt.set_type_of_expr(expr.id, then_ty.clone());
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
                self.ctxt.set_type_of_expr(expr.id, my_type.clone());
                Ok(my_type)
            },
            ExprKind::Ref(value) => {
                let inner_ty = self.visit_expr(value)?;
                let ty = self.ctxt.new_ref_ty(inner_ty);
                self.ctxt.set_type_of_expr(expr.id, ty.clone());
                Ok(ty)
            },
            ExprKind::Deref(value) => {
                let actual = self.visit_expr(value)?;
                let my_type = self.ctxt.new_type_var();
                let expected = self.ctxt.new_ref_ty(my_type.clone());
                self.ctxt.unify(&actual, &expected)?;
                self.ctxt.set_type_of_expr(expr.id, my_type.clone());
                Ok(my_type)
            },
            ExprKind::Block(block) => self.visit_block(block),
            ExprKind::Ret(value) => {
                match value {
                    Some(v) => self.visit_expr(v),
                    None => Ok(self.ctxt.common_types.unit.clone()),
                }
            },
            ExprKind::Struct(strukt) => todo!(),
            ExprKind::Field(strukt, field) => todo!(),
            ExprKind::Closure(name, args, body) => {
                let arg_types = args.iter().map(|e| self.visit_expr(e)).collect::<Result<Vec<_>,_>>()?;

                if let Some(name) = name {
                    // if name is some, then this is a recursive closure. We bind the name preemptively and then typecheck the body
                    let preemptive_ret = self.ctxt.new_type_var();
                    todo!()
                }
                else {
                    let body_ty = self.visit_func_block(body)?;
                    let ret = self.ctxt.new_function_ty(arg_types, body_ty);
                    self.ctxt.set_type_of_expr(expr.id, ret.clone());
                    Ok(ret)
                }
            }
        }
    }

    fn visit_ident(&mut self, ident: &Identifier) -> Self::Result {
        // instantiate
        todo!()
    }

    fn visit_block(&mut self, block: &Block) -> Self::Result {
        todo!()
    }

    fn visit_func_block(&mut self, block: &FuncBlock) -> Self::Result {
        todo!()
    }

    fn visit_literal(&mut self, literal: &Literal) -> Self::Result {
        Ok(match literal {
            Literal::Int(_) => self.ctxt.common_types.int.clone(),
            Literal::Float(_) => self.ctxt.common_types.float.clone(),
            Literal::Char(_) => self.ctxt.common_types.char.clone(),
            Literal::Str(_) => panic!("Strings NYI"),
            Literal::Bool(_) => self.ctxt.common_types.bool.clone(),
        })
    }
}