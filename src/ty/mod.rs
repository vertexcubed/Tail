

pub struct Ty {
    pub id: usize,
    pub kind: TyKind,
}

pub enum TyKind {
    Bool,
    Char,
    Int,
    Float,
    Struct(Vec<Ty>),
    Ref(Box<Ty>),
    Infer(InferTy),
    Function(Vec<Ty>, Box<Ty>),
}

pub enum InferTy {
    TyVar(usize),
    FreshTyVar(usize)
}