mod visit;

pub struct Identifier;


pub struct Expr {
    pub id: usize,
    pub kind: ExprKind,
    pub span: Span,
}

pub struct Span;

pub enum ExprKind {
    Lit,
    UnaryOp,
    BinaryOp,
    If(Box<Expr>, Box<Block>, Box<Block>),
    Call,
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Block(Box<Block>),
    Ret(Option<Box<Expr>>),
    Struct(Box<StructExpr>),
    Field(Box<Expr>, Identifier)
}

pub struct Block {
    pub stmts: Vec<Stmt>
}


pub struct Stmt {
    pub id: usize,
    pub kind: StmtKind,
    pub span: Span,
}

pub enum StmtKind {
    Let(Identifier, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Expr(Box<Expr>)
}

pub struct StructExpr {
    // TODO: replace with longid
    pub name: Identifier,
    pub fields: Vec<ExprField>
}

pub struct ExprField {
    pub key: Identifier,
    pub value: Box<Expr>
}