use crate::ast::*;

/// An AST Visitor. This is heavily subject to change as I figure out a better way of implementing the visitor pattern in Rust
pub trait AstVisitor {
    type ExprResult;
    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::ExprResult;
    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult;
    fn visit_ident(&mut self, ident: &Identifier) -> Self::ExprResult;
    fn visit_block(&mut self, block: &Block) -> Self::ExprResult;
    fn visit_func_block(&mut self, block: &FuncBlock) -> Self::ExprResult;
    fn visit_literal(&mut self, literal: &Literal) -> Self::ExprResult;
}