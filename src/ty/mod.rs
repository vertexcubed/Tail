use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Sub};
use crate::ast::NodeId;
use crate::impl_id;
use crate::ty::infer::InferCtxt;

pub mod visit;
pub mod infer;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TyId(usize);
impl_id!(TyId);

#[derive(Debug, Clone)]
pub struct Ty {
    pub id: TyId,
    pub kind: TyKind
}
impl Ty {
    // most of the time you dont want to be calling this
    pub fn new(id: impl Into<TyId>, kind: TyKind) -> Self {
        Self {
            id: id.into(),
            kind,
        }
    }
}
impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{} ({})", self.id, self.kind)
    }
}


#[derive(Debug, Clone)]
pub enum TyKind {
    Int,
    Float,
    Bool,
    Char,
    Unit,
    Struct(Vec<Ty>),
    Ref(Box<Ty>),
    Infer(InferTy),
    Function(Vec<Ty>, Box<Ty>),
}
impl Display for TyKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self)
    }
}

#[derive(Debug, Clone)]
pub enum InferTy {
    TyVar,
    FreshTyVar
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum TyError {
    #[error("Unification error")]
    InvalidTy(#[from] UnifyError)
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum UnifyError {
    #[error("Cannot unify {0} with {1}")]
    InvalidUnify(Ty, Ty),
    #[error("Function {0} and function {1} have non-matching arities")]
    InvalidArity(Ty, Ty),
    #[error("Type {0} occurs in type {1}")]
    OccursIn(Ty, Ty),
}

#[derive(Debug)]
pub struct TyCtxt {
    node_types: HashMap<NodeId, Ty>,
    infer_ctxt: InferCtxt,
    pub common_types: CommonTypes,
}

impl TyCtxt {

    pub fn new() -> TyCtxt {

        let mut infer_ctxt = InferCtxt::new();
        let unit = Ty::new(infer_ctxt.reserve_id(), TyKind::Unit);
        let int = Ty::new(infer_ctxt.reserve_id(), TyKind::Int);
        let float = Ty::new(infer_ctxt.reserve_id(), TyKind::Float);
        let bool = Ty::new(infer_ctxt.reserve_id(), TyKind::Bool);
        let char = Ty::new(infer_ctxt.reserve_id(), TyKind::Char);

        infer_ctxt.add_type(unit.clone());
        infer_ctxt.add_type(int.clone());
        infer_ctxt.add_type(float.clone());
        infer_ctxt.add_type(bool.clone());
        infer_ctxt.add_type(char.clone());
        let common_types = CommonTypes {
            int,
            float,
            bool,
            char,
            unit
        };
        Self {
            infer_ctxt: InferCtxt::new(),
            node_types: HashMap::new(),
            common_types,
        }
    }

    pub fn get_type_of_expr(&self, node: NodeId) -> Option<&Ty> {
        self.node_types.get(&node)
    }

    pub fn set_type_of_expr(&mut self, node: NodeId, ty: Ty) {
        self.node_types.insert(node, ty);
    }

    pub fn unify(&mut self, left: &Ty, right: &Ty) -> Result<(), TyError> {
        self.infer_ctxt.unify(left, right).map_err(|u| TyError::InvalidTy(u))
    }

    pub fn new_type_var(&mut self) -> Ty {
        let id = self.infer_ctxt.reserve_id();
        let out = Ty::new(id, TyKind::Infer(InferTy::TyVar));
        self.infer_ctxt.add_type(out.clone());
        out
    }

    pub fn new_function_ty(&mut self, args: Vec<Ty>, ret: Ty) -> Ty {
        let id = self.infer_ctxt.reserve_id();
        let out = Ty::new(id, TyKind::Function(args, Box::new(ret)));
        self.infer_ctxt.add_type(out.clone());
        out
    }

    pub fn new_ref_ty(&mut self, value: Ty) -> Ty {
        let id = self.infer_ctxt.reserve_id();
        let out = Ty::new(id, TyKind::Ref(Box::new(value)));
        self.infer_ctxt.add_type(out.clone());
        out
    }
}

#[derive(Debug)]
pub struct CommonTypes {
    pub int: Ty,
    pub float: Ty,
    pub bool: Ty,
    pub char: Ty,
    pub unit: Ty,
}