use std::any::TypeId;
use std::collections::HashMap;
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

#[derive(Debug, Clone)]
pub enum InferTy {
    TyVar,
    FreshTyVar
}


pub enum TyError {

}

#[derive(Debug)]
pub struct TyCtxt {
    node_types: HashMap<NodeId, Ty>,
    infer_ctxt: InferCtxt,
    pub common_types: CommonTypes,
    next_var_id: TyId,
}

impl TyCtxt {

    pub fn new() -> TyCtxt {

        let unit = Ty::new(0, TyKind::Unit);
        let int = Ty::new(1, TyKind::Int);
        let float = Ty::new(2, TyKind::Float);
        let bool = Ty::new(3, TyKind::Bool);
        let char = Ty::new(4, TyKind::Char);
        let next_var_id = TyId(5);
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
            next_var_id
        }
    }

    pub fn get_type_of_expr(&self, node: NodeId) -> Option<&Ty> {
        self.node_types.get(&node)
    }

    pub fn set_type_of_expr(&mut self, node: NodeId, ty: Ty) {
        self.node_types.insert(node, ty);
    }

    pub fn unify(&mut self, left: &Ty, right: &Ty) -> Result<(), TyError> {
        todo!()
    }

    pub fn new_type_var(&mut self) -> Ty {
        let id = self.next_var_id;
        self.next_var_id += 1;
        Ty::new(id, TyKind::Infer(InferTy::TyVar))
    }

    pub fn new_function_ty(&mut self, args: Vec<Ty>, ret: Ty) -> Ty {
        let id = self.next_var_id;
        self.next_var_id += 1;
        Ty::new(id, TyKind::Function(args, Box::new(ret)))
    }

    pub fn new_ref_ty(&mut self, value: Ty) -> Ty {
        let id = self.next_var_id;
        self.next_var_id += 1;
        Ty::new(id, TyKind::Ref(Box::new(value)))
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