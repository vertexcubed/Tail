use std::collections::HashMap;
use crate::ty::{InferTy, Ty, TyError, TyId, TyKind, UnifyError};

#[derive(Debug)]
pub struct InferCtxt {
    all_types: HashMap<TyId, Ty>,
    table: UnificationTable,
    next_var_id: TyId,
}

impl InferCtxt {
    pub fn new() -> Self {
        Self {
            all_types: HashMap::new(),
            table: UnificationTable::new(),
            next_var_id: TyId(0)
        }
    }

    pub fn reserve_id(&mut self) -> TyId {
        let id = self.next_var_id;
        self.next_var_id += 1;
        id
    }

    pub fn add_type(&mut self, ty: Ty) {
        let id_num = ty.id.into();
        self.all_types.insert(ty.id, ty);
        self.table.reserve_to(id_num);
        self.table.vec[id_num] = id_num;
    }

    pub fn find<'ctx>(&'ctx mut self, ty: &'ctx Ty) -> &'ctx Ty {
        match &ty.kind {
            TyKind::Infer(InferTy::TyVar) => {
                let id = self.table.find(ty.id.0);
                &self.all_types[&TyId(id)]
            },
            _ => ty,
        }
    }

    pub fn unify(&mut self, lhs: &Ty, rhs: &Ty) -> Result<(), UnifyError> {

        // lot of cloning going on but it doesnt really matter that much
        let (lhs, rhs) = (self.find(lhs).clone(), self.find(rhs).clone());

        // literally the same, we can skip
        if lhs.id == rhs.id { return Ok(()); }

        match (&lhs.kind, &rhs.kind) {
            // These cases might be able to be commented out cuz they get covered by the above?
            (TyKind::Unit, TyKind::Unit) => Ok(()),
            (TyKind::Int, TyKind::Int) => Ok(()),
            (TyKind::Bool, TyKind::Bool) => Ok(()),
            (TyKind::Float, TyKind::Float) => Ok(()),
            (TyKind::Char, TyKind::Char) => Ok(()),
            // TODO: make this row polymorphic
            (TyKind::Struct(l_types), TyKind::Struct(r_types)) => todo!(),


            (TyKind::Ref(l), TyKind::Ref(r)) => self.unify(l, r),
            (TyKind::Function(l_args, l_ret), TyKind::Function(r_args, r_ret)) => {
                if l_args.len() != r_args.len() {
                    Err(UnifyError::InvalidArity(lhs.clone(), rhs.clone()))
                }
                else {
                    for (l, r) in l_args.iter().zip(r_args.iter()) {
                        self.unify(l, r)?;
                    }
                    self.unify(l_ret, r_ret)
                }
            }

            (TyKind::Infer(InferTy::TyVar), TyKind::Infer(InferTy::TyVar)) => {
                // no occurs check needed when its just two type variables
                // non tyvar types should never be unioned theoretically so its safe to do no checks here?
                self.table.union(lhs.id.into(), rhs.id.into());
                Ok(())
            }

            // lhs is type var
            (TyKind::Infer(InferTy::TyVar), _) => {
                // occurs check. check if the left hand side occurs in the right hand sight
                if self.occurs(&lhs, &rhs) {
                    return Err(UnifyError::OccursIn(lhs, rhs));
                }
                // update lh tyvar to point to rh var
                self.table.union(lhs.id.into(), rhs.id.into());
                Ok(())
            },

            // rhs is type var
            (_, TyKind::Infer(InferTy::TyVar)) => {
                // occurs check. check if the right hand side occurs in the left hand side
                if self.occurs(&rhs, &lhs) {
                    return Err(UnifyError::OccursIn(rhs, lhs));
                }
                // update rh tyvar to point to lh var
                self.table.union(rhs.id.into(), lhs.id.into());
                Ok(())
            },

            _ => Err(UnifyError::InvalidUnify(lhs.clone(), rhs.clone()))
        }
    }

    fn occurs(&mut self, query: &Ty, inside: &Ty) -> bool {
        // if query is not a type var, then we dont really care lol
        let TyKind::Infer(InferTy::TyVar) = query.kind else {
            return false;
        };
        match &inside.kind {
            TyKind::Int | TyKind::Float | TyKind::Bool | TyKind::Char | TyKind::Unit => false,
            TyKind::Struct(fields) => {
                for f in fields.iter() {
                    if self.occurs(query, f) {
                        return true;
                    }
                }
                false
            }
            TyKind::Ref(value) => self.occurs(query, value),
            TyKind::Infer(_) => query.id == inside.id,
            TyKind::Function(args, ret) => {
                for a in args.iter() {
                    if self.occurs(query, a) {
                        return true;
                    }
                }
                self.occurs(query, ret)
            }
        }

    }
}

#[derive(Debug, Clone)]
struct UnificationTable {
    vec: Vec<usize>
}

impl UnificationTable {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    // simple union find w/ path compression
    pub fn find(&mut self, key: usize) -> usize {
        self.reserve_to(key);
        let mut root = key;
        while self.vec[root] != root {
            root = self.vec[root];
        }

        let mut iter = key;
        while self.vec[iter] != root {
            let temp_parent = self.vec[iter];
            self.vec[iter] = root;
            iter = temp_parent;
        }

        root
    }

    //TODO: consider using union by size?
    pub fn union(&mut self, lhs: usize, rhs: usize) {
        let l_root = self.find(lhs);
        let r_root = self.find(rhs);
        if l_root == r_root {
            return;
        }

        // update lhs to point to rhs
        self.vec[l_root] = r_root;
    }

    fn reserve_to(&mut self, value: usize) {
        let old_len = self.vec.len();
        if old_len < (value + 1) {
            let new_items = (value + 1) - old_len;
            self.vec.reserve(new_items);
            for i in old_len..=value {
                self.vec.push(i);
            }
        }
    }
}