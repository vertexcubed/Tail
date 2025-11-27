use crate::ast::*;

pub trait AstVisitor {
    type Result;
    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result;
    fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
    fn visit_ident(&mut self, ident: &Identifier) -> Self::Result;
    fn visit_block(&mut self, block: &Block) -> Self::Result;
    fn visit_func_block(&mut self, block: &FuncBlock) -> Self::Result;
    fn visit_literal(&mut self, literal: &Literal) -> Self::Result;
}