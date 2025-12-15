use crate::ast;
use crate::ast::*;
use crate::vm::def::{CaptureDef, FunctionDef};
use crate::vm::vm::TailVirtualMachine;
use crate::vm::Instruction::*;
use crate::vm::{CodeChunk, ConstantPoolEntry, SourceFile};
use std::rc::Rc;
use std::sync::Mutex;

// This is super bad but it doesn't really matter because it's only for testing code
static COUNTER: Mutex<usize> = Mutex::new(0);

pub fn init_counter() {
    *COUNTER.lock().unwrap() = 0;
}

fn new_id() -> NodeId {
    let mut lock = COUNTER.lock().unwrap();
    let id = *lock;
    *lock += 1;
    NodeId(id)
}

fn let_(name: &str, arg: impl Into<Expr>) -> Stmt {
    Stmt {
        id: new_id(),
        kind: StmtKind::Let(
            ast::Identifier(name.to_string()),
            Box::new(arg.into())
        )
    }
}

fn assign(name: &str, arg: impl Into<Expr>) -> Stmt {
    ass_place(_place(name), arg)
}

fn ass_place(left: PlaceExpr, right: impl Into<Expr>) -> Stmt {
    Stmt {
        id: new_id(),
        kind: StmtKind::Assign(left, Box::new(right.into()))
    }
}
impl Expr {
    fn to_stmt(self) -> Stmt {
        Stmt {
            id: new_id(),
            kind: StmtKind::Expr(Box::new(self))
        }
    }

    fn add(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Add, self, other)
    }
    fn sub(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Sub, self, other)
    }
    fn mul(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Mul, self, other)
    }
    fn div(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Div, self, other)
    }
    fn mod_(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Mod, self, other)
    }
    fn lt(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Lt, self, other)
    }
    fn lte(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Lte, self, other)
    }
    fn gt(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Gt, self, other)
    }
    fn gte(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Gte, self, other)
    }
    fn eq(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Eq, self, other)
    }
    fn neq(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::NEq, self, other)
    }
    fn and(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::And, self, other)
    }
    fn or(self, other: impl Into<Expr>) -> Expr {
        bop(BinOp::Or, self, other)
    }

}
impl Into<Stmt> for Expr {
    fn into(self) -> Stmt {
        self.to_stmt()
    }
}

fn ret(value: impl Into<Expr>) -> Stmt {
    Stmt {
        id: new_id(),
        kind: StmtKind::Ret(Some(Box::new(value.into())))
    }
}
fn ret_unit() -> Stmt {
    Stmt {
        id: new_id(),
        kind: StmtKind::Ret(None)
    }
}

fn block(stmts: Vec<impl Into<Stmt>>) -> Block {
    Block { stmts: stmts.into_iter().map(|s| s.into()).collect() }
}
impl Block {
    fn _as_expr(self) -> Expr {
        Expr {
            id: new_id(),
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
        self._as_expr().to_stmt()
    }
}
impl Into<Expr> for i64 {
    fn into(self) -> Expr {
        int(self)
    }
}
impl Into<Expr> for bool {
    fn into(self) -> Expr {
        bool(self)
    }
}
impl Into<Expr> for () {
    fn into(self) -> Expr {
        unit()
    }
}


fn int(value: i64) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::Lit(Literal::Int(value))
    }
}
fn bool(value: bool) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::Lit(Literal::Bool(value))
    }
}
fn unit() -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::Lit(Literal::Unit)
    }
}
fn ident(name: &str) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::Ident(ast::Identifier(name.to_string()))
    }
}
fn uop(op: UOp, expr: impl Into<Expr>) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::UnaryOp(op, Box::new(expr.into()))
    }
}
fn not(expr: impl Into<Expr>) -> Expr {
    uop(UOp::Not, expr)
}
fn neg(expr: impl Into<Expr>) -> Expr {
    uop(UOp::Not, expr)
}

fn bop(op: BinOp, lhs: impl Into<Expr>, rhs: impl Into<Expr>) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::BinaryOp(op, Box::new(lhs.into()), Box::new(rhs.into()))
    }
}
fn if_(cond: impl Into<Expr>, then: Vec<impl Into<Stmt>>, else_: Vec<impl Into<Stmt>>) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::If(Box::new(cond.into()), Box::new(block(then)), Box::new(block(else_)))
    }
}



fn clos(name: Option<&str>, args: Vec<&str>, code: Vec<impl Into<Stmt>>) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::Closure(
            name.map(|n| ast::Identifier(n.to_string())),
            args.iter().map(|a| ast::Identifier(a.to_string())).collect(),
            Box::new(FuncBlock { stmts: code.into_iter().map(|s| s.into()).collect() })
        )
    }
}

fn func(name: &str, args: Vec<&str>, code: Vec<impl Into<Stmt>>) -> Stmt {
    let_(name, clos(Some(name), args, code))
}

fn call(func: &str, args: Vec<impl Into<Expr>>) -> Expr {
    call_expr(ident(func), args)
}

fn call_expr(func: impl Into<Expr>, args: Vec<impl Into<Expr>>) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::Call(Box::new(func.into()), args.into_iter().map(|a| a.into()).collect())
    }
}
fn ref_(expr: impl Into<Expr>) -> Expr {
    Expr {
        id: new_id(),
        kind: ExprKind::Ref(Box::new(expr.into()))
    }
}
fn deref(inner: impl Into<Expr>, num_derefs: usize) -> PlaceExpr {
    PlaceExpr {
        inner: Box::new(inner.into()),
        derefs: num_derefs,
        fields: Vec::new()
    }
}
fn _place(iden: &str) -> PlaceExpr {
    deref(ident(iden), 0)
}
impl PlaceExpr {
    fn as_expr(self) -> Expr {
        Expr {
            id: new_id(),
            kind: ExprKind::Place(self)
        }
    }
}
impl Into<Expr> for PlaceExpr {
    fn into(self) -> Expr {
        self.as_expr()
    }
}

fn empty_stmts() -> Vec<Stmt> {
    Vec::new()
}
fn empty_args() -> Vec<Expr> {
    Vec::new()
}



//==================//
// Testing Functions//
//==================//


pub fn factorial() -> Vec<Stmt> {
    /*
    let factorial(n) = {
        if n <= 1 {
            return 1
        }
        return n * factorial(n - 1)
    }
    let a = factorial(5)
    let b = factorial(10)
    let d = factorial(2)
    */
    vec![
        func("factorial", vec!["n"], vec![
            if_(ident("n").lte(int(1)), vec![
                ret(int(1))
            ], empty_stmts()).to_stmt(),
            ret(ident("n").mul(call(
                "factorial",
                vec![ident("n").sub(int(1))]
            )))
        ]),

        let_("a", call("factorial", vec![int(5)])),
        let_("b", call("factorial", vec![int(10)])),
        let_("d", call("factorial", vec![int(2)])),
    ]
}


pub fn basic_refs() -> Vec<Stmt> {
    /*
    let addtwo(val) =
        *val = *val + 2
    }
    let a = ref 2
    let b = ref 3
    addtwo(a)
    addtwo(b)
    addtwo(b)
    addtwo(a)
     */
    vec![
        func("addtwo", vec!["val"], vec![
            ass_place(
                deref(ident("val"), 1),
                deref(ident("val"), 1).as_expr().add(int(2))),
        ]),

        let_("a", ref_(int(2))),
        let_("b", ref_(int(3))),
        call("addtwo", vec![ident("a")]).into(),
        call("addtwo", vec![ident("b")]).into(),
        call("addtwo", vec![ident("b")]).into(),
        call("addtwo", vec![ident("a")]).into(),
    ]
}

// Note: This code may not work in the future
pub fn basic_closure() -> Vec<Stmt> {
    /*
    // Temporary functions, because panic!() not implemented yet
    let foo() = { return -10 }
    let bar() = { return -10 }

    let a = 2
    {
        let b = 20
        let a = 3
        foo = fun() -> {
            a = a + 4
            return a
        }
        bar = fun() -> {
            a = a - 2
            return a
        }
    }
    {
        let a = 20
        let b = foo()
        let b = foo()
        let b = bar()
        let b = foo()
        let b = bar()
    }
     */
    vec![
        func("foo", vec![], vec![
            ret(int(-10))
        ]),
        func("bar", vec![], vec![
            ret(int(-10))
        ]),

        let_("a", int(2)),
        block(vec![
            let_("b", int(20)),
            let_("a", int(3)),
            assign("foo", clos(None, vec![], vec![
                assign("a", ident("a").add(int(4))),
                ret(ident("a"))
            ])),
            assign("bar", clos(None, vec![], vec![
                assign("a", ident("a").sub(int(2))),
                ret(ident("a"))
            ])),
        ]).into(),
        block(vec![
            let_("a", int(20)),
            let_("b", call("foo", empty_args())),
            let_("b", call("foo", empty_args())),
            let_("b", call("bar", empty_args())),
            let_("b", call("foo", empty_args())),
            let_("b", call("bar", empty_args())),
        ]).into(),
    ]
}


pub fn complex_closure() -> Vec<Stmt> {

    /*
    let make() = {
        let a = 3
        let inner() = {
            a = a + 2
        }
        return inner
    }
    let a = make()
    let b = make()
    a()
    b()
    b()
    a()
    b()
     */
    vec![
        func("make", vec![], vec![
            let_("a", int(3)),
            func("inner", vec![], vec![
                assign("a", ident("a").add(int(2))),
            ]),
            ret(ident("inner"))
        ]),


        let_("a", call("make", empty_args())),
        let_("b", call("make", empty_args())),
        // _call("a", empty_args()).into(),
        call("a", empty_args()).into(),
        call("b", empty_args()).into(),
        call("b", empty_args()).into(),
        call("a", empty_args()).into(),
        call("b", empty_args()).into(),
    ]
}

pub fn bad_fib() -> Vec<Stmt> {
    /*
    let fib(n) = {
        return if n <= 1 {
            n
        } else {
            fib(n - 1) + fib(n - 2)
        }
    }
    let a = fib(30)
     */
    vec![
        func("fib", vec!["n"], vec![
            ret(
                if_(ident("n").lte(int(1)), vec![
                    ident("n")
                ], vec![
                    call("fib", vec![ident("n").sub(1)]).add(call("fib", vec![ident("n").sub(2)]))
                ])
            ),
        ]),

        let_("a", call("fib", vec![int(30)])),
    ]
}
// Essentially the same code as basic_closure, but as Bytecode
pub fn bin_basic_closure() {

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
// Essentially the same code as complex_closure, but as Bytecode
pub fn bin_complex_closure() {

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


pub fn slides_code() -> Vec<Stmt> {
    /*
    let factorial(n) = {
        if n <= 1 {
            return 1
        }
        return n * factorial(n - 1)
    }

    let add_to(value) = {
        *value += 5
    }

    let foo = ref 2
    add_to(foo)
    *foo += factorial(10)
    */
    vec![
        func("factorial", vec!["n"], vec![
            if_(ident("n").lte(1), vec![
                ret(1)
            ], empty_stmts()).to_stmt(),
            ret(ident("n").mul(call("factorial", vec![ident("n").sub(1)])))
        ]),
        func("add_to", vec!["value"], vec![
            ass_place(
                deref(ident("value"), 1),
                deref(ident("value"), 1).as_expr().add(5))
        ]),
        let_("foo", ref_(2)),
        call("add_to", vec![ident("foo")]).into(),
        ass_place(
            deref(ident("foo"), 1),
            deref(ident("foo"), 1).as_expr().add(
                call("factorial", vec![10])
            )),
    ]
}


pub fn unit_values() -> Vec<Stmt> {
    vec![
        // store unit into x
        let_("x", ()),
        // push 2, pop it
        block(vec![
            int(2),
        ])._as_expr().into(),
        // do nothing
        block(empty_stmts())._as_expr().into(),
        // assign unit to x
        assign("x", ()),
        // do nothing
        ident("x").into(),
        // do nothing
        unit().into(),
        // store unit into y
        let_("y", ident("x")),
        func("meow", vec!["a", "unit"], vec![
            assign("a", 4),
            ret(ident("unit"))
        ]),
        // this should push unit and pop it
        call("meow", vec![int(2), unit()]).into(),
        func("other", vec![], vec![
            ret_unit()
        ]),
        // this should not push unit
        call("other", empty_args()).into(),
    ]
}