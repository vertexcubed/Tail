use crate::impl_id;
use std::fmt::{Display, Formatter};

pub mod visit;


#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);
impl_id!(NodeId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Represents a singular expression. Has an ID and a kind
#[derive(Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    // pub span: Span,
}

// TODO: NYI
#[derive(Debug)]
pub struct Span;

#[derive(Debug)]
pub enum ExprKind {
    /// A literal.
    Lit(Literal),
    /// A variable identifier
    Ident(Identifier),
    /// A Unary operator, such as Negate or Not.
    UnaryOp(UOp, Box<Expr>),
    /// A binary operator, such as Add or Sub.
    BinaryOp(BinOp, Box<Expr>, Box<Expr>),
    /// An if statement. If statements are expressions, meaning they return values.
    /// No need for a ternary operator because of this.
    If(Box<Expr>, Box<Block>, Box<Block>),
    /// A closure/function definition. Identifier is None for anonymous lambdas and corresponds to the function name in let bound closures.
    Closure(Option<Identifier>, Vec<Identifier>, Box<FuncBlock>),
    /// Function application. Tail does not support currying.
    Call(Box<Expr>, Vec<Expr>),
    /// The reference operator.
    Ref(Box<Expr>),
    /// A block of statements.
    Block(Box<Block>),
    /// A struct constructor
    Struct(StructExpr),
    /// A place expression, i.e. Derefs and struct lookups.
    Place(PlaceExpr)
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
    Unit,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

/// A FuncBlock differs from regular blocks in that the last expression is not the type of the block, rather it's the type of return statements in the block.
#[derive(Debug)]
pub struct FuncBlock {
    pub stmts: Vec<Stmt>
}


#[derive(Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    // pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    /// Let expressions.
    Let(Identifier, Box<Expr>),
    /// Assignment operations.
    Assign(PlaceExpr, Box<Expr>),
    /// A singular expression where the value is discarded.
    Expr(Box<Expr>),
    /// A return call. None for Unit functions
    Ret(Option<Box<Expr>>),
}

#[derive(Debug)]
pub struct StructExpr {
    // TODO: replace with longid
    pub name: Identifier,
    pub fields: Vec<ExprField>
}

#[derive(Debug)]
pub struct PlaceExpr {
    // in an assign operation, this should ALWAYS be an identifier
    pub inner: Box<Expr>,
    pub derefs: usize,
    pub fields: Vec<Identifier>
}



#[derive(Debug)]
pub struct ExprField {
    pub key: Identifier,
    pub value: Box<Expr>
}