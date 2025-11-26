use crate::ast::{Expr, ExprKind, Stmt};

pub trait Visitor {
    type Result;
    fn visit_statement(&mut self, stmt: &Stmt) -> Self::Result;
    fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
}

pub trait Visitable<V: Visitor> {
    fn accept(&self, visitor: &mut V) -> V::Result;
}