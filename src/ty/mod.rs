use crate::ast::Identifier;
use crate::impl_id;
use crate::ty::infer::InferCtxt;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use std::ops::{Add, Sub};

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
    #[error("Unification error: {0}")]
    InvalidTy(#[from] UnifyError),
    #[error("Unbound variable: {0}")]
    UnboundVar(Identifier)
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
    local: LocalCtxt,
    func: FuncCtxt,
    infer_ctxt: InferCtxt,
    pub common_types: CommonTypes,
    scopes: VecDeque<LocalCtxt>,
    func_scopes: VecDeque<FuncCtxt>,
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
            local: LocalCtxt::new(),
            func: FuncCtxt::default(),
            infer_ctxt,
            // node_types: HashMap::new(),
            common_types,
            scopes: VecDeque::new(),
            func_scopes: VecDeque::new()
        }
    }

    // enters a scope
    pub fn enter_scope(&mut self) {
        let old_scope = self.local.clone();
        self.scopes.push_back(old_scope);
    }
    // enters a function scope. The main difference is it resets the return type
    pub fn enter_func_scope(&mut self) {
        self.enter_scope();

        let old_func = std::mem::replace(&mut self.func, FuncCtxt::default());
        self.func_scopes.push_back(old_func);
    }

    // exits a scope
    pub fn exit_scope(&mut self) {
        self.local = self.scopes.pop_back().unwrap();
    }

    pub fn exit_func_scope(&mut self) {
        self.exit_scope();
        self.func = self.func_scopes.pop_back().unwrap();
    }




    // TODO: shadowing, let polymorphism
    pub fn bind(&mut self, id: Identifier, ty: Ty) {
        self.local.bind(id, ty)
    }

    pub fn get_binding(&self, id: &Identifier) -> Result<Ty, TyError> {
        self.local.get_binding(id)
    }

    pub fn bindings(&self) -> &HashMap<Identifier, Ty> {
        &self.local.bindings
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

    pub fn unify_ret(&mut self, ty: &Option<Ty>) -> Result<(), TyError> {
        match (&self.func.ret, ty) {
            (Some(_), None) => {},
            (None, Some(r)) => {
                self.func.ret = Some(r.clone());
            }
            (Some(l), Some(r)) => {
                // clone required because mutable + immutable refs :')
                self.unify(&l.clone(), r)?;
            }
            (None, None) => {},
        }
        Ok(())
    }

    pub fn get_ret_ty(&self) -> Option<Ty> {
        self.func.ret.clone()
    }
    pub fn write_ty(&mut self, ty: &Ty, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        let ty = self.infer_ctxt.find(ty).clone();
        match ty.kind {
            TyKind::Int => {
                write!(f, "int")
            }
            TyKind::Float => { write!(f, "float")}
            TyKind::Bool => { write!(f, "bool")}
            TyKind::Char => { write!(f, "char")}
            TyKind::Unit => { write!(f, "unit")}
            TyKind::Struct(s) => {
                write!(f, "struct {{")?;
                for t in s.iter() {
                    self.write_ty(t, f)?;
                };
                write!(f, "}}")
            }
            TyKind::Ref(r) => {
                write!(f, "ref ")?;
                self.write_ty(r.as_ref(), f)
            }
            TyKind::Infer(_) => { write!(f, "'{}", ty.id)}
            TyKind::Function(args, ret) => {
                write!(f, "fun(")?;
                for i in 0..args.len() {
                    self.write_ty(&args[i], f)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> ")?;
                self.write_ty(ret.as_ref(), f)
            }
        }
    }
}
#[derive(Debug, Clone, Default)]
pub struct FuncCtxt {
    ret: Option<Ty>,
}


#[derive(Debug, Clone)]
pub struct LocalCtxt {
    bindings: HashMap<Identifier, Ty>,
}
impl LocalCtxt {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn bind(&mut self, id: Identifier, ty: Ty) {
        // do something, TODO
        if self.bindings.contains_key(&id) {

        }
        self.bindings.insert(id, ty);
    }

    pub fn get_binding(&self, id: &Identifier) -> Result<Ty, TyError> {
        self.bindings.get(id).cloned().ok_or(TyError::UnboundVar(id.clone()))
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