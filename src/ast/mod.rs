use crate::impl_id;

pub mod visit;


#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);
impl_id!(NodeId);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier;

#[derive(Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Span;

#[derive(Debug)]
pub enum ExprKind {
    Lit(Literal),
    Ident(Identifier),
    UnaryOp(UOp, Box<Expr>),
    BinaryOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Block>, Box<Block>),
    Closure(Option<Identifier>, Vec<Expr>, Box<FuncBlock>),
    Call(Box<Expr>, Vec<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Block(Box<Block>),
    Ret(Option<Box<Expr>>),
    Struct(Box<StructExpr>),
    Field(Box<Expr>, Identifier)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UOp {
    Not,
    Neg,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    NEq,
}



#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
    Str(String),
    Bool(bool),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

#[derive(Debug)]
pub struct FuncBlock {
    pub stmts: Vec<Stmt>
}


#[derive(Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Let(Identifier, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Expr(Box<Expr>)
}

#[derive(Debug)]
pub struct StructExpr {
    // TODO: replace with longid
    pub name: Identifier,
    pub fields: Vec<ExprField>
}

#[derive(Debug)]
pub struct ExprField {
    pub key: Identifier,
    pub value: Box<Expr>
}