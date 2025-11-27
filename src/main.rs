use std::rc::Rc;
use crate::vm::{CodeChunk, StackValue};
use crate::vm::Instruction::*;
use crate::vm::vm::TailVirtualMachine;

mod vm;
mod ast;
mod ty;


macro_rules! impl_id {
    ($typ:ty) => {
        impl std::ops::Add for $typ {
            type Output = $typ;
            fn add(self, rhs: Self) -> Self::Output {
                Self(self.0 + rhs.0)
            }
        }
        impl std::ops::Sub for $typ {
            type Output = $typ;
            fn sub(self, rhs: Self) -> Self::Output {
                Self(self.0 - rhs.0)
            }
        }
        impl std::ops::AddAssign<$typ> for $typ {
            fn add_assign(&mut self, rhs: Self) {
                self.0 += rhs.0;
            }
        }
        impl std::ops::SubAssign<$typ> for $typ {
            fn sub_assign(&mut self, rhs: Self) {
                self.0 -= rhs.0;
            }
        }
        impl std::ops::Add<usize> for $typ {
            type Output = $typ;
            fn add(self, rhs: usize) -> Self::Output {
                Self(self.0 + rhs)
            }
        }
        impl std::ops::Sub<usize> for $typ {
            type Output = $typ;
            fn sub(self, rhs: usize) -> Self::Output {
                Self(self.0 - rhs)
            }
        }
        impl std::ops::AddAssign<usize> for $typ {
            fn add_assign(&mut self, rhs: usize) {
                self.0 += rhs;
            }
        }
        impl std::ops::SubAssign<usize> for $typ {
            fn sub_assign(&mut self, rhs: usize) {
                self.0 -= rhs;
            }
        }
        impl Into<usize> for $typ {
            fn into(self) -> usize {
                self.0
            }
        }
        impl From<usize> for $typ {
            fn from(value: usize) -> Self {
                Self(value)
            }
        }
        impl std::fmt::Display for $typ {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}
pub(crate) use impl_id;
use crate::ast::{BinOp, Expr, ExprKind, Literal, NodeId, Span};
use crate::ast::visit::AstVisitor;
use crate::ty::visit::TypeVisitor;

fn main() {

    let expr = Expr {
        id: NodeId(0),
        kind: ExprKind::BinaryOp(
            BinOp::Add,
            Box::new(Expr {
                id: NodeId(1),
                kind: ExprKind::BinaryOp(
                    BinOp::Mul,
                    Box::new(Expr {
                        id: NodeId(2),
                        kind: ExprKind::Lit(Literal::Float(2.0)),
                        span: Span,
                    }),
                    Box::new(Expr {
                        id: NodeId(2),
                        kind: ExprKind::Lit(Literal::Int(5)),
                        span: Span,
                    })
                ),
                span: Span,
            }),
            Box::new(Expr {
                id: NodeId(4),
                kind: ExprKind::Lit(Literal::Int(3)),
                span: Span,
            })
        ),
        span: Span,
    };

    let mut visitor = TypeVisitor::new();
    println!("{:?}", visitor.visit_expr(&expr));









    let mut vm = TailVirtualMachine::new();

    // let other_code = CodeChunk::new(vec![
    //     Load1,
    //     IPush(24),
    //     IMul,
    //     Ret,
    // ]);
    //
    // let other = StackValue::Function(1, &other_code);

    // let other_idx = vm.add_to_constant_pool(other);

    // let func_code = CodeChunk::new(vec![
    //     Load1,
    //     Ldc(other_idx),
    //     Call,
    //     Load2,
    //     IAdd,
    //     Ret,
    // ]);
    // let func = StackValue::Function(2, &func_code);

    // let func_idx = vm.add_to_constant_pool(func);

    // let a = 2
    // let b = 3
    // let c = b - a


    let instrs = vec![
        IPush2,
        Store0,
        IPush3,
        Store1,
        Load0,
        Load1,
        ISub,
        Store2
    ];
    let chunk = CodeChunk::new(instrs);

    vm.run(Rc::new(chunk));

    // func_code lives at least as long as the vm
}