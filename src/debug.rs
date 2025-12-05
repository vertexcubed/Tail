// Functions for making ASTs by hand. Solely for debugging

use std::ops::{Add, Div, Mul, Sub};
use std::sync::Mutex;
use crate::ast;
use crate::ast::{BinOp, Block, Expr, ExprKind, FuncBlock, Literal, NodeId, PlaceExpr, Stmt, StmtKind, UOp};


// This is super bad but it doesn't really matter because it's only for testing code
static COUNTER: Mutex<usize> = Mutex::new(0);

pub fn init_counter() {
    *COUNTER.lock().unwrap() = 0;
}

fn _new_id() -> NodeId {
    let mut lock = COUNTER.lock().unwrap();
    let id = *lock;
    *lock += 1;
    NodeId(id)
}

fn _let(name: &str, arg: Expr) -> Stmt {
    Stmt {
        id: _new_id(),
        kind: StmtKind::Let(
            ast::Identifier(name.to_string()),
            Box::new(arg)
        )
    }
}

fn _assign(name: &str, arg: Expr) -> Stmt {
    _ass_place(_place(name), arg)
}

fn _ass_place(left: PlaceExpr, right: Expr) -> Stmt {
    Stmt {
        id: _new_id(),
        kind: StmtKind::Assign(left, Box::new(right))
    }
}
impl Expr {
    fn _as_stmt(self) -> Stmt {
        Stmt {
            id: _new_id(),
            kind: StmtKind::Expr(Box::new(self))
        }
    }

    fn _add(self, other: Expr) -> Expr {
        _bop(BinOp::Add, self, other)
    }
    fn _sub(self, other: Expr) -> Expr {
        _bop(BinOp::Sub, self, other)
    }
    fn _mul(self, other: Expr) -> Expr {
        _bop(BinOp::Mul, self, other)
    }
    fn _div(self, other: Expr) -> Expr {
        _bop(BinOp::Div, self, other)
    }
    fn _mod(self, other: Expr) -> Expr {
        _bop(BinOp::Mod, self, other)
    }
    fn _lt(self, other: Expr) -> Expr {
        _bop(BinOp::Lt, self, other)
    }
    fn _lte(self, other: Expr) -> Expr {
        _bop(BinOp::Lte, self, other)
    }
    fn _gt(self, other: Expr) -> Expr {
        _bop(BinOp::Gt, self, other)
    }
    fn _gte(self, other: Expr) -> Expr {
        _bop(BinOp::Gte, self, other)
    }
    fn _eq(self, other: Expr) -> Expr {
        _bop(BinOp::Eq, self, other)
    }
    fn _neq(self, other: Expr) -> Expr {
        _bop(BinOp::NEq, self, other)
    }
    fn _and(self, other: Expr) -> Expr {
        _bop(BinOp::And, self, other)
    }
    fn _or(self, other: Expr) -> Expr {
        _bop(BinOp::Or, self, other)
    }

}
impl Into<Stmt> for Expr {
    fn into(self) -> Stmt {
        self._as_stmt()
    }
}

fn _ret(value: Expr) -> Stmt {
    Stmt {
        id: _new_id(),
        kind: StmtKind::Ret(Some(Box::new(value)))
    }
}
fn _ret_unit() -> Stmt {
    Stmt {
        id: _new_id(),
        kind: StmtKind::Ret(None)
    }
}

fn _block(stmts: Vec<Stmt>) -> Block {
    Block { stmts }
}
impl Block {
    fn _as_expr(self) -> Expr {
        Expr {
            id: _new_id(),
            kind: ExprKind::Block(Box::new(self))
        }
    }
}
impl Into<Expr> for Block {
    fn into(self) -> Expr {
        self._as_expr()
    }
}
impl Into<Stmt> for Block {
    fn into(self) -> Stmt {
        self._as_expr()._as_stmt()
    }
}



fn _int(value: i64) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::Lit(Literal::Int(value))
    }
}
fn _bool(value: bool) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::Lit(Literal::Bool(value))
    }
}
fn _ident(name: &str) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::Ident(ast::Identifier(name.to_string()))
    }
}
fn _uop(op: UOp, expr: Expr) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::UnaryOp(op, Box::new(expr))
    }
}
fn _not(expr: Expr) -> Expr {
    _uop(UOp::Not, expr)
}
fn _neg(expr: Expr) -> Expr {
    _uop(UOp::Not, expr)
}

fn _bop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::BinaryOp(op, Box::new(lhs), Box::new(rhs))
    }
}
fn _if(cond: Expr, then: Vec<Stmt>, else_: Vec<Stmt>) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::If(Box::new(cond), Box::new(_block(then)), Box::new(_block(else_)))
    }
}



fn _clos(name: Option<&str>, args: Vec<&str>, code: Vec<Stmt>) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::Closure(
            name.map(|n| ast::Identifier(n.to_string())),
            args.iter().map(|a| ast::Identifier(a.to_string())).collect(),
            Box::new(FuncBlock { stmts: code })
        )
    }
}

fn _func(name: &str, args: Vec<&str>, code: Vec<Stmt>) -> Stmt {
    _let(name, _clos(Some(name), args, code))
}

fn _call(func: &str, args: Vec<Expr>) -> Expr {
    _call_expr(_ident(func), args)
}

fn _call_expr(func: Expr, args: Vec<Expr>) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::Call(Box::new(func), args)
    }
}
fn _ref(expr: Expr) -> Expr {
    Expr {
        id: _new_id(),
        kind: ExprKind::Ref(Box::new(expr))
    }
}
fn _deref(inner: Expr, num_derefs: usize) -> PlaceExpr {
    PlaceExpr {
        inner: Box::new(inner),
        derefs: num_derefs,
        fields: Vec::new()
    }
}
fn _place(ident: &str) -> PlaceExpr {
    _deref(_ident(ident), 0)
}












pub fn factorial() -> Vec<Stmt> {
    vec![
        _func("factorial", vec!["n"], vec![
            _if(_ident("n")._lte(_int(1)), vec![
                _ret(_int(1))
            ], vec![])._as_stmt(),
            _ret(_ident("n")._mul(_call(
                "factorial",
                vec![_ident("n")._sub(_int(1))]
            )))
        ]),

        _let("a", _call("factorial", vec![_int(5)])),
        _let("b", _call("factorial", vec![_int(10)])),
        _let("d", _call("factorial", vec![_int(2)])),
    ]
}




pub fn basic_closure() -> Vec<Stmt> {
    vec![
        _func("foo", vec![], vec![
            _ret(_int(-10))
        ]),
        _func("bar", vec![], vec![
            _ret(_int(-10))
        ]),

        _let("a", _int(2)),
        _block(vec![
            _let("b", _int(20)),
            _let("a", _int(3)),
            _assign("foo", _clos(None, vec![], vec![
                _assign("a", _ident("a")._add(_int(4))),
                _ret(_ident("a"))
            ])),
            _assign("bar", _clos(None, vec![], vec![
                _assign("a", _ident("a")._sub(_int(2))),
                _ret(_ident("a"))
            ])),
        ]).into(),
        _block(vec![
            _let("a", _int(20)),
            _let("meow", _call("foo", vec![])),
            _let("meow", _call("foo", vec![])),
            _let("meow", _call("bar", vec![])),
            _let("meow", _call("foo", vec![])),
            _let("meow", _call("bar", vec![])),
        ]).into(),
    ]
}


pub fn higher_order_closures() -> Vec<Stmt> {
    vec![
        _func("make", vec![], vec![
            _let("a", _int(3)),
            _func("inner", vec![], vec![
                _assign("a", _ident("a")._add(_int(2))),
            ]),
            _ret(_ident("inner"))
        ]),


        _let("a", _call("make", vec![])),
        _let("b", _call("make", vec![])),
        _call("a", vec![]).into(),
        _call("a", vec![]).into(),
        _call("b", vec![]).into(),
        _call("b", vec![]).into(),
        _call("a", vec![]).into(),
        _call("b", vec![]).into(),
    ]
}