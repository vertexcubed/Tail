use std::rc::Rc;
use crate::vm::{CodeChunk, ConstantPoolEntry, SourceFile};
use crate::vm::Instruction::*;
use crate::vm::vm::TailVirtualMachine;

mod vm;
mod ast;
mod ty;
mod parse;
mod compile;
mod debug;

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
use crate::ast::{BinOp, Block, Expr, ExprKind, FuncBlock, Literal, NodeId, Stmt, StmtKind};
use crate::ast::visit::AstVisitor;
use crate::compile::visit::Compiler;
use crate::ty::visit::TypeVisitor;
use crate::vm::def::{CaptureDef, FunctionDef};

fn test_ast() {

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
                        kind: ExprKind::Lit(Literal::Int(2)),
                        // span: Span,
                    }),
                    Box::new(Expr {
                        id: NodeId(2),
                        kind: ExprKind::Lit(Literal::Int(5)),
                        // span: Span,
                    })
                ),
                // span: Span,
            }),
            Box::new(Expr {
                id: NodeId(4),
                kind: ExprKind::Lit(Literal::Int(3)),
                // span: Span,
            })
        ),
        // span: Span,
    };

    let mut visitor = TypeVisitor::new();
    println!("{:?}", visitor.visit_expr(&expr));

    // fun(int, 'a, int) -> int
    let func = Stmt {
        id: NodeId(5),
        // let main = fun(x, y, z) -> {}
        kind: StmtKind::Let(
            ast::Identifier("main".to_string()),
            Box::new(Expr {
                id: NodeId(6),
                kind: ExprKind::Closure(
                    Some(ast::Identifier("main".to_string())),
                    vec![
                        ast::Identifier("x".to_string()),
                        ast::Identifier("y".to_string()),
                        ast::Identifier("z".to_string()),
                    ],
                    Box::new(FuncBlock { stmts: vec![
                        // if .. {} else {}
                        Stmt {
                            id: NodeId(7),
                            kind: StmtKind::Expr(Box::new(Expr {
                                id: NodeId(8),
                                kind: ExprKind::If(
                                    // x < 5
                                    Box::new(Expr {
                                        id: NodeId(9),
                                        kind: ExprKind::BinaryOp(
                                            BinOp::Lt,
                                            Box::new(Expr {
                                                id: NodeId(10),
                                                kind: ExprKind::Ident(ast::Identifier("x".to_string()))
                                            }),
                                            Box::new(Expr {
                                                id: NodeId(11),
                                                kind: ExprKind::Lit(Literal::Int(5))
                                            })
                                        )
                                    }),
                                    // { return x }
                                    Box::new(Block { stmts: vec![
                                        Stmt {
                                            id: NodeId(12),
                                            kind: StmtKind::Ret(Some(Box::new(Expr {
                                                id: NodeId(13),
                                                kind: ExprKind::Ident(ast::Identifier("x".to_string()))
                                            })))
                                        }
                                    ]}),
                                    // else {}
                                    Box::new(Block { stmts: vec![ ]})
                                )
                            }))
                        },
                        // return z
                        Stmt {
                            id: NodeId(14),
                            kind: StmtKind::Ret(Some(Box::new(Expr {
                                id: NodeId(15),
                                kind: ExprKind::Ident(ast::Identifier("z".to_string()))
                            })))
                        }
                    ]}),
                )
            })
        )
    };

    let id = Expr {
        id: NodeId(16),
        kind: ExprKind::Ident(ast::Identifier("main".to_string())),
    };

    let fact = Stmt {
        id: NodeId(17),
        kind: StmtKind::Let(
            ast::Identifier("factorial".to_string()),
            Box::new(Expr {
                id: NodeId(18),
                kind: ExprKind::Closure(
                    Some(ast::Identifier("factorial".to_string())),
                    vec![
                        ast::Identifier("n".to_string()),
                    ],
                    Box::new(FuncBlock { stmts: vec![
                        // if n <= 1 { return 1 }
                        Stmt {
                            id: NodeId(19),
                            kind: StmtKind::Expr(Box::new(Expr {
                                id: NodeId(20),
                                kind: ExprKind::If(
                                    Box::new(Expr {
                                        id: NodeId(21),
                                        kind: ExprKind::BinaryOp(
                                            BinOp::Lte,
                                            Box::new(Expr {
                                                id: NodeId(22),
                                                kind: ExprKind::Ident(ast::Identifier("n".to_string()))
                                            }),
                                            Box::new(Expr {
                                                id: NodeId(23),
                                                kind: ExprKind::Lit(Literal::Int(1))
                                            })
                                        )
                                    }),
                                    Box::new(Block { stmts: vec![
                                        Stmt {
                                            id: NodeId(24),
                                            kind: StmtKind::Ret(Some(Box::new(Expr {
                                                id: NodeId(25),
                                                kind: ExprKind::Lit(Literal::Int(1))
                                            })))
                                        }
                                    ]}),
                                    Box::new(Block { stmts: vec![ ]}),
                                )
                            }))
                        },
                        // let out = factorial(n - 1)
                        Stmt {
                            id: NodeId(26),
                            kind: StmtKind::Let(
                                ast::Identifier("out".to_string()),
                                Box::new(Expr {
                                    id: NodeId(27),
                                    kind: ExprKind::Call(
                                        Box::new(Expr {
                                            id: NodeId(28),
                                            kind: ExprKind::Ident(ast::Identifier("factorial".to_string())),
                                        }),
                                        vec![Expr {
                                            id: NodeId(29),
                                            kind: ExprKind::BinaryOp(
                                                BinOp::Sub,
                                                Box::new(Expr {
                                                    id: NodeId(30),
                                                    kind: ExprKind::Ident(ast::Identifier("n".to_string())),
                                                }),
                                                Box::new(Expr {
                                                    id: NodeId(31),
                                                    kind: ExprKind::Lit(Literal::Int(1))
                                                }),
                                            )
                                        }]
                                    )
                                })
                            )
                        },
                        // return out
                        Stmt {
                            id: NodeId(32),
                            kind: StmtKind::Ret(Some(Box::new(Expr {
                                id: NodeId(33),
                                kind: ExprKind::Ident(ast::Identifier("out".to_string())),
                            })))
                        }
                    ]})
                )
            })
        )
    };

    let fact_func = Expr {
        id: NodeId(34),
        kind: ExprKind::Ident(ast::Identifier("factorial".to_string())),
    };











    println!("{:?}", visitor.visit_stmt(&func));
    let expr_ty = visitor.visit_expr(&id);
    println!("{:?}", visitor.visit_stmt(&fact));
    let fact_ty = visitor.visit_expr(&fact_func);

    if let Ok(t) = expr_ty {
        let mut str = String::new();
        visitor.ctxt.write_ty(&t, &mut str);
        println!("{}", str);
    }
    else {
        println!("{:?}", expr_ty)
    }
    
    if let Ok(t) = fact_ty {
        let mut str = String::new();
        visitor.ctxt.write_ty(&t, &mut str);
        println!("{}", str);
    }
    else {
        println!("{:?}", fact_ty)
    }







    let to_compile = vec![
        Stmt {
            id: NodeId(0),
            kind: StmtKind::Let(
                ast::Identifier("x".to_string()),
                Box::new(Expr {
                    id: NodeId(1),
                    kind: ExprKind::Lit(Literal::Int(2))
                })
            )
        },
        Stmt {
            id: NodeId(2),
            kind: StmtKind::Let(
                ast::Identifier("y".to_string()),
                Box::new(Expr {
                    id: NodeId(3),
                    kind: ExprKind::Lit(Literal::Int(1))
                })
            )
        },
        Stmt {
            id: NodeId(4),
            kind: StmtKind::Let(
                ast::Identifier("z".to_string()),
                Box::new(Expr {
                    id: NodeId(5),
                    kind: ExprKind::BinaryOp(
                        BinOp::Sub,
                        Box::new(Expr {
                            id: NodeId(6),
                            kind: ExprKind::Ident(ast::Identifier("y".to_string())),
                        }),
                        Box::new(Expr {
                            id: NodeId(7),
                            kind: ExprKind::Ident(ast::Identifier("x".to_string()))
                        })
                    )
                })
            )
        },
    ];


    let mut compile_types = TypeVisitor::new();
    for s in to_compile.iter() {
        if let Err(e) = compile_types.visit_stmt(s){
            println!("{}", e);
            return;
        }
    }

    let mut compile_code = Compiler::new();
    for s in to_compile.iter() {
        if let Err(e) = compile_code.visit_stmt(s) {
            println!("Something borke")
        }
    }
    let source_file = compile_code.build();

    let mut vm = TailVirtualMachine::new(&source_file);
    vm.run();
}


fn basic_upvalues() {

    let constant_pool = vec![
        ConstantPoolEntry::FunctionDef(Rc::new(FunctionDef {
            arity: 0,
            code_chunk: 0
        })),
        ConstantPoolEntry::FunctionDef(Rc::new(FunctionDef {
            arity: 0,
            code_chunk: 1
        })),
        ConstantPoolEntry::FunctionDef(Rc::new(FunctionDef {
            arity: 0,
            code_chunk: 2
        }))
    ];

    let empty = CodeChunk::new(vec![
        Ret
    ]);

    let main = CodeChunk::new(vec![
        // let foo = fun() -> {panic!()}
        ClosPush(0, vec![]),
        GStore0,
        // let bar = fun() -> {panic!()}
        ClosPush(0, vec![]),
        GStore1,

        // let a = 2
        IPush2,
        GStore0,

        // { ...
        // let b = 4
        IPush4,
        Store0,
        // let a = 3
        IPush3,
        Store1,
        ClosPush(1, vec![CaptureDef::local(1)]),
        GStore0,
        ClosPush(2, vec![CaptureDef::local(1)]),
        GStore1,

        // ... }
        // Clear(0),
        // Clear(1),

        // { ...
        // let a = 20
        IPush(20),
        Store0,
        // foo()
        GLoad0,
        Call,
        // foo()
        GLoad0,
        Call,
        // bar()
        GLoad1,
        Call,
        // foo()
        GLoad0,
        Call,
        // bar()
        GLoad1,
        Call,
        // bar()
        GLoad1,
        Call,
        // bar()
        GLoad1,
        Call,
        // bar()
        GLoad1,
        Call,

        // ... }
        // Clear(0)
    ]);

    let foo = CodeChunk::new(vec![
        IPush4,
        UpLoad(0),
        IAdd,
        UpStore(0),
        UpLoad(0),
        Ret,
    ]);

    let bar = CodeChunk::new(vec![
        IPush2,
        UpLoad(0),
        ISub,
        UpStore(0),
        UpLoad(0),
        Ret
    ]);


    let source = SourceFile {
        constant_pool,
        main_code: main,
        functions: vec![
            empty,
            foo,
            bar
        ]
    };

    let mut vm = TailVirtualMachine::new(&source);
    vm.run();
}

fn complex_upvalues() {

    let constants = vec![
        ConstantPoolEntry::FunctionDef(Rc::new(FunctionDef {
            arity: 0,
            code_chunk: 0,
        })),
        ConstantPoolEntry::FunctionDef(Rc::new(FunctionDef {
            arity: 0,
            code_chunk: 1,
        })),
    ];


    let make = CodeChunk::new(vec![
        IPush2,
        Store1,

        ClosPush(1, vec![CaptureDef::local(1)]),
        Store2,
        Load2,
        Ret
    ]);

    let inner = CodeChunk::new(vec![
        IPush1,
        UpLoad(0),
        IAdd,
        UpStore(0),
        UpLoad(0),
        Ret,
    ]);


    let main = CodeChunk::new(vec![
        // let make() = {...}
        ClosPush(0, vec![]),
        GStore0,
        // let a = make()
        GLoad0,
        Call,
        GStore1,
        // let b = make()
        GLoad0,
        Call,
        GStore2,

        //a()
        GLoad1,
        Call,
        //a()
        GLoad1,
        Call,

        // b()
        GLoad2,
        Call,
        // b()
        GLoad2,
        Call,
        // b()
        GLoad2,
        Call,
        //a()
        GLoad1,
        Call,
        // b()
        GLoad2,
        Call,
        //a()
        GLoad1,
        Call,
        // b()
        GLoad2,
        Call,
    ]);

    let file = SourceFile {
        constant_pool: constants,
        main_code: main,
        functions: vec![
            make,
            inner,
        ]
    };

    let mut vm = TailVirtualMachine::new(&file);
    vm.run();
}

fn if_test() {
    let code = vec![
        Stmt {
            id: NodeId(0),
            kind: StmtKind::Let(
                ast::Identifier("x".to_string()),
                Box::new(Expr {
                    id: NodeId(1),
                    kind: ExprKind::Lit(Literal::Int(32))
                })
            )
        },
        Stmt {
            id: NodeId(2),
            kind: StmtKind::Let(
                ast::Identifier("y".to_string()),
                Box::new(Expr {
                    id: NodeId(3),
                    kind: ExprKind::If(
                        Box::new(Expr {
                            id: NodeId(4),
                            kind: ExprKind::BinaryOp(
                                BinOp::Lt,
                                Box::new(Expr {
                                    id: NodeId(5),
                                    kind: ExprKind::Ident(ast::Identifier("x".to_string()))
                                }),
                                Box::new(Expr {
                                    id: NodeId(6),
                                    kind: ExprKind::Lit(Literal::Int(8))
                                })
                            )
                        }),
                        Box::new(Block { stmts: vec![
                            Stmt {
                                id: NodeId(7),
                                kind: StmtKind::Expr(Box::new(Expr {
                                    id: NodeId(8),
                                    kind: ExprKind::Lit(Literal::Int(-5))
                                }))
                            }
                        ]}),
                        Box::new(Block { stmts: vec![
                            Stmt {
                                id: NodeId(9),
                                kind: StmtKind::Let(
                                    ast::Identifier("y".to_string()),
                                    Box::new(Expr {
                                        id: NodeId(10),
                                        kind: ExprKind::Block(Box::new(Block {stmts: vec![
                                            Stmt {
                                                id: NodeId(11),
                                                kind: StmtKind::Let(
                                                    ast::Identifier("x".to_string()),
                                                    Box::new(Expr {
                                                        id: NodeId(12),
                                                        kind: ExprKind::Lit(Literal::Bool(false))
                                                    })
                                                )
                                            },
                                    ]}))
                                }))
                            },
                            Stmt {
                                id: NodeId(13),
                                kind: StmtKind::Expr(Box::new(Expr {
                                    id: NodeId(14),
                                    kind: ExprKind::Lit(Literal::Int(0))
                                }))
                            }
                        ]})
                    )
                })
            )
        }
    ];


}

fn run_code(code: impl FnOnce() -> Vec<Stmt>) {
    debug::init_counter();
    let stmts = code();

    println!("Type checking...");
    let mut type_visitor = TypeVisitor::new();
    for s in stmts.iter() {
        match type_visitor.visit_stmt(s) {
            // almost always this is unit so its not actually that useful
            Ok(_) => {}
            Err(e) => {
                println!("{}", e);
                return;
            }
        }
    }
    for (id, ty) in type_visitor.ctxt.bindings().clone().into_iter() {
        let mut str = String::new();
        let _ = type_visitor.ctxt.write_ty(&ty, &mut str);
        println!("val {}: {}", id, str);
    }

    println!("Compiling...");

    let mut compiler = Compiler::new();
    for s in stmts.iter() {
        compiler.visit_stmt(s).unwrap();
    }
    // this might be bad
    let source = compiler.build();

    println!("Printing source file\n{}", source);

    println!("Executing...");

    let mut vm = TailVirtualMachine::new(&source);
    vm.run();
}


fn main() {
    run_code(debug::basic_refs);
}